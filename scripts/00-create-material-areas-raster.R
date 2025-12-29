# -----------------------------------------------------------------------------
# Script: calculate_material_areas.R
# Location: ./scripts/
# Description: 
#   1. Reads mining spatial data (cluster_features.gpkg).
#   2. Processes multi-material polygons (splits area equally among materials).
#   3. Calculates area per pixel in HECTARES (weighted by material share).
#   4. Applies a scale factor (x100) to preserve precision (0.01 ha).
#   5. Saves as a compressed, multi-layer 32-bit INTEGER raster (.tif).
#   6. Generates a complete metadata file describing the scaling and units.
# -----------------------------------------------------------------------------

library(sf)
library(terra)
library(dplyr)
library(stringr)
library(tidyr)

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------
# Inferred from 04-cluster-extenssions-and-overview.R
DATA_VERSION <- "20251030-all_materials" 
INPUT_FILE   <- file.path("output", DATA_VERSION, "cluster_features.gpkg")

# Raster Settings
SCALE_FACTOR <- 100 
DATA_TYPE    <- "INT4S" # Signed 32-bit integer (Required for 50km cells where area > 327ha)
TARGET_RES   <- 0.5     # ~55km (30 arc-minutes) at equator

# -----------------------------------------------------------------------------
# 2. Load and Pre-process Data
# -----------------------------------------------------------------------------
message("Loading data from: ", INPUT_FILE)

if (!file.exists(INPUT_FILE)) {
  stop("Input file not found. Please ensure '", INPUT_FILE, "' exists.")
}

# Read spatial data
# We only want Polygons (ID starts with 'A'), not Points ('P')
pols_raw <- st_read(INPUT_FILE, quiet = TRUE) %>%
  filter(str_starts(id, "A"))

message("Preprocessing attributes...")

# Expand list of materials (comma separated) into individual rows
# Logic matches 'material_expanded' in the reference script
pols_expanded <- pols_raw %>%
  select(id, primary_materials_list) %>%
  separate_rows(primary_materials_list, sep = ",\\s*") %>%
  mutate(primary_material = if_else(is.na(primary_materials_list), "Unknown", primary_materials_list)) %>%
  group_by(id) %>%
  mutate(n_materials = n()) %>% # Count how many materials per polygon for weighting
  ungroup()

unique_materials <- unique(pols_expanded$primary_material)
message(paste("Found", length(unique_materials), "unique materials."))

# -----------------------------------------------------------------------------
# 3. Define Target Raster Grid
# -----------------------------------------------------------------------------
message("Defining target grid...")

# Define grid based on extent of data
r_template <- rast(ext(pols_raw), resolution = TARGET_RES, crs = crs(pols_raw))

# Calculate cell area in HECTARES (ha)
# unit="km" gives km2. Multiply by 100 to get hectares.
cell_areas_ha <- cellSize(r_template, unit = "km") * 100

# -----------------------------------------------------------------------------
# 4. Create Multi-layer Raster
# -----------------------------------------------------------------------------
message("Calculating area per pixel...")

raster_layers <- list()

# Helper function to rasterize weighted coverage
# We must process polygons with different weights separately because 
# rasterize(cover=TRUE) does not support a weight field directly.
for (mat in unique_materials) {
  message(paste("Processing material:", mat))
  
  # Initialize an empty accumulator raster for this material
  r_mat_accum <- rast(r_template)
  values(r_mat_accum) <- 0
  
  # Filter for this material
  mat_subset <- pols_expanded %>% filter(primary_material == mat)
  
  # Get unique weight groups (e.g., polygons shared by 1, 2, or 3 materials)
  weight_groups <- unique(mat_subset$n_materials)
  
  for (n in weight_groups) {
    # Get polygons that share this 'n' count
    pols_n <- mat_subset %>% filter(n_materials == n)
    
    if (nrow(pols_n) > 0) {
      # Calculate physical coverage fraction (0 to 1)
      r_cov <- rasterize(pols_n, r_template, cover = TRUE, background = 0)
      
      # Apply weight (1/n)
      # If a polygon is Copper & Gold (n=2), Copper gets 50% of the area.
      r_weighted <- r_cov * (1 / n)
      
      # Add to accumulator
      r_mat_accum <- r_mat_accum + r_weighted
    }
  }
  
  # Convert Fraction to Area (ha)
  r_area_ha <- r_mat_accum * cell_areas_ha
  
  # Apply Scale Factor and Round to Integer
  r_int <- round(r_area_ha * SCALE_FACTOR)
  
  names(r_int) <- mat
  raster_layers[[mat]] <- r_int
}

final_stack <- rast(raster_layers)

# -----------------------------------------------------------------------------
# 5. Write Raster Output (Optimized)
# -----------------------------------------------------------------------------
output_tif <- "./scripts/mining_materials_area_int.tif"
message(paste("Writing optimized raster to:", output_tif))

# Write as INT4S with LZW compression
writeRaster(final_stack, output_tif, 
            overwrite = TRUE, 
            datatype = DATA_TYPE,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2")) 

# -----------------------------------------------------------------------------
# 6. Verification: Area Check
# -----------------------------------------------------------------------------
message("Performing area verification check...")

# 1. Calculate Total Polygon Area (in Hectares)
# st_area usually returns m^2 for geographic coordinates. 1 ha = 10,000 m^2
total_poly_area_m2 <- sum(st_area(pols_raw))
total_poly_area_ha <- as.numeric(total_poly_area_m2) / 10000

# 2. Calculate Total Raster Area (in Hectares)
# Sum all values across all layers, then divide by SCALE_FACTOR
raster_global_stats <- global(final_stack, "sum", na.rm = TRUE)
total_raster_int    <- sum(raster_global_stats$sum)
total_raster_area_ha <- total_raster_int / SCALE_FACTOR

# 3. Calculate Difference
diff_ha  <- total_raster_area_ha - total_poly_area_ha
diff_pct <- (diff_ha / total_poly_area_ha) * 100

message(sprintf("Total Polygon Area: %12.2f ha", total_poly_area_ha))
message(sprintf("Total Raster Area:  %12.2f ha", total_raster_area_ha))
message(sprintf("Difference:         %12.2f ha (%0.4f%%)", diff_ha, diff_pct))

# -----------------------------------------------------------------------------
# 7. Generate Metadata
# -----------------------------------------------------------------------------
output_meta <- "./scripts/mining_materials_area_metadata.txt"
message(paste("Writing metadata to:", output_meta))

info_date <- Sys.time()
info_crs <- if (!is.na(crs(final_stack, describe=TRUE)$name)) crs(final_stack, describe=TRUE)$name else "EPSG:4326"
info_layers <- names(final_stack)

metadata_content <- c(
  "===============================================================================",
  " METADATA: MINING COMMODITY AREA RASTER (OPTIMIZED 50KM)",
  "===============================================================================",
  "",
  "1. GENERAL INFORMATION",
  "----------------------",
  paste0("Filename:        ", basename(output_tif)),
  paste0("Source Data:     ", INPUT_FILE),
  paste0("Date Generated:  ", info_date),
  paste0("Data Type:       ", DATA_TYPE, " (Signed 32-bit Integer)"),
  "",
  "2. DATA INTERPRETATION (CRITICAL)",
  "---------------------------------",
  "Unit:               Hectares (ha)",
  paste0("Scale Factor:       ", SCALE_FACTOR),
  "Precision:          0.01 ha",
  "",
  "*** HOW TO READ VALUES ***",
  paste0("To get the physical area in Hectares, DIVIDE the raster value by ", SCALE_FACTOR, "."),
  paste0("  Formula: Area (ha) = Raster_Value / ", SCALE_FACTOR),
  paste0("  Example: A value of 525 means 5.25 hectares."),
  "",
  "3. VALIDATION STATISTICS",
  "------------------------",
  paste0("Total Source Polygon Area: ", sprintf("%.2f ha", total_poly_area_ha)),
  paste0("Total Output Raster Area:  ", sprintf("%.2f ha", total_raster_area_ha)),
  paste0("Difference:                ", sprintf("%.2f ha (%.4f%%)", diff_ha, diff_pct)),
  "Note: Small differences are expected due to rasterization effects.",
  "",
  "4. METHODOLOGY NOTE",
  "-------------------",
  "Polygons containing multiple commodities have their area split equally among",
  "all listed commodities. For example, a 10ha polygon listing 'Copper, Gold'",
  "contributes 5ha to the Copper layer and 5ha to the Gold layer.",
  "",
  "5. SPATIAL REFERENCE",
  "--------------------",
  paste0("Coordinate System: ", info_crs),
  paste0("Resolution:        ", paste(res(final_stack), collapse = " x "), " (~55km at equator)"),
  "",
  "6. LAYER LIST",
  "-------------"
)

for (i in seq_along(info_layers)) {
  metadata_content <- c(metadata_content, paste0("Band ", i, ": ", info_layers[i]))
}

writeLines(metadata_content, output_meta)

# -----------------------------------------------------------------------------
# 8. Rank Check
# -----------------------------------------------------------------------------
message("\n--- Ranking of Mining Area per Material (Derived from Raster) ---")

# Sum all pixels per layer
layer_sums <- global(final_stack, "sum", na.rm = TRUE)

# Create summary dataframe
material_rank <- data.frame(
  material_layer = names(final_stack),
  total_value = layer_sums$sum
) %>%
  mutate(
    area_ha = total_value / SCALE_FACTOR,
    percentage = (area_ha / sum(area_ha)) * 100
  ) %>%
  arrange(desc(area_ha)) %>%
  mutate(
    area_ha = sprintf("%.2f", area_ha),
    percentage = sprintf("%.2f%%", percentage)
  ) %>%
  select(Biome = material_layer, Area_Ha = area_ha, Percent = percentage)

print(material_rank, row.names = FALSE)

message("Done.")
