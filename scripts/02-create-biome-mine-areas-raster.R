# -----------------------------------------------------------------------------
# Script: calculate_biome_areas.R
# Location: ./scripts/
# Description: 
#   1. Reads mining spatial data (cluster_features.gpkg).
#   2. Assigns Biome/Ecoregion info to each polygon (using Ecoregions2017).
#      - Checks for existing 'ecoregion_tbl.csv'.
#      - If missing, downloads and processes Ecoregions2017.zip.
#   3. Creates a multi-layer raster where each layer is a Biome.
#      Pixel values = Area of mining in that biome (Hectares).
#   4. Scales values (x100) and saves as 32-bit Integer (INT4S).
#   5. Generates metadata.
# -----------------------------------------------------------------------------

library(sf)
library(terra)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# Create output directories
dir.create("./scripts", showWarnings = FALSE)
dir.create("./tmp/ecoregions", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------
DATA_VERSION <- "20251030-all_materials" 
# Adjust paths to match repository structure
INPUT_GPKG    <- file.path("output", DATA_VERSION, "cluster_features.gpkg")
ECO_TBL_PATH  <- file.path("output", DATA_VERSION, "ecoregion_tbl.csv")
ECO_SHP_PATH  <- "./tmp/ecoregions/Ecoregions2017.shp"
ECO_URL       <- "https://storage.googleapis.com/teow2016/Ecoregions2017.zip"

# Raster Settings (Consistent with 50km Material Raster)
SCALE_FACTOR <- 100 
DATA_TYPE    <- "INT4S" # Signed 32-bit integer
TARGET_RES   <- 0.5     # ~55km (30 arc-minutes)

# -----------------------------------------------------------------------------
# 2. Load Mining Data
# -----------------------------------------------------------------------------
message("Loading mining data from: ", INPUT_GPKG)
if (!file.exists(INPUT_GPKG)) {
  stop("Input file not found: ", INPUT_GPKG)
}

# Load only Polygons (ID starts with 'A')
pols_raw <- st_read(INPUT_GPKG, quiet = TRUE) %>% 
  filter(str_starts(id, "A"))

# -----------------------------------------------------------------------------
# 3. Assign Biomes (Logic adapted from 04-cluster-extenssions-and-overview.R)
# -----------------------------------------------------------------------------
message("Assigning Biomes to polygons...")

# Helper to ensure valid geometries for joins
sf_use_s2(FALSE) 

if (file.exists(ECO_TBL_PATH)) {
  message("Found existing ecoregion table. Loading...")
  eco_tbl <- read_csv(ECO_TBL_PATH, show_col_types = FALSE)
  
  # Join to polygons
  pols_joined <- left_join(pols_raw, eco_tbl, by = "id")

} else {
  message("Ecoregion table not found. Processing Ecoregions2017 source...")
  
  # Check if Shapefile exists, if not download
  if (!file.exists(ECO_SHP_PATH)) {
    message("Downloading Ecoregions2017.zip...")
    dest_zip <- "./tmp/Ecoregions2017.zip"
    download.file(ECO_URL, destfile = dest_zip, mode = "wb")
    unzip(dest_zip, exdir = "./tmp/ecoregions/")
  }
  
  message("Reading Ecoregions shapefile...")
  eco_sf <- st_read(ECO_SHP_PATH, quiet = TRUE) %>%
    select(ecoregion_name = ECO_NAME, biome_name = BIOME_NAME) %>%
    st_make_valid() 
  
  message("Spatial Join: Nearest Feature (Centroids)...")
  # Using centroids matches the logic in the reference script (04...R)
  pols_centroids <- st_centroid(pols_raw)
  
  pols_joined_sf <- st_join(pols_centroids, eco_sf, join = st_nearest_feature)
  
  # Extract table and merge back to original polygons
  eco_tbl_new <- pols_joined_sf %>%
    st_drop_geometry() %>%
    select(id, ecoregion_name, biome_name)
  
  pols_joined <- left_join(pols_raw, eco_tbl_new, by = "id")
}

# Handle NAs and clean names for Raster Layers
pols_ready <- pols_joined %>%
  mutate(biome_name = replace_na(biome_name, "Unknown")) %>%
  mutate(biome_layer = str_replace_all(biome_name, "[^[:alnum:]]", "_")) %>%
  mutate(biome_layer = str_replace_all(biome_layer, "_+", "_")) %>%
  mutate(biome_layer = str_remove_all(biome_layer, "^_|_$"))

unique_biomes <- unique(pols_ready$biome_layer)
message(paste("Found", length(unique_biomes), "unique biomes."))

# -----------------------------------------------------------------------------
# 4. Define Target Grid
# -----------------------------------------------------------------------------
message("Defining target grid...")
r_template <- rast(ext(pols_ready), resolution = TARGET_RES, crs = crs(pols_ready))

# Calculate cell area in HECTARES (ha)
cell_areas_ha <- cellSize(r_template, unit = "km") * 100

# -----------------------------------------------------------------------------
# 5. Create Multi-layer Raster
# -----------------------------------------------------------------------------
message("Calculating area per pixel for each biome...")

raster_layers <- list()

for (b_name in unique_biomes) {
  message(paste("Processing biome:", b_name))
  
  # Filter polygons for this biome
  pols_sub <- pols_ready %>% filter(biome_layer == b_name)
  
  if (nrow(pols_sub) > 0) {
    # Rasterize: Fraction of coverage (0-1)
    # Unlike materials, each polygon has only 1 biome, so no weighting needed.
    r_cov <- rasterize(pols_sub, r_template, cover = TRUE, background = 0)
    
    # Area = Fraction * Cell Area
    r_area_ha <- r_cov * cell_areas_ha
    
    # Scale and Round
    r_int <- round(r_area_ha * SCALE_FACTOR)
    
    names(r_int) <- b_name
    raster_layers[[b_name]] <- r_int
  }
}

final_stack <- rast(raster_layers)

# -----------------------------------------------------------------------------
# 6. Verification
# -----------------------------------------------------------------------------
message("Performing validation...")
total_poly_area_ha <- sum(as.numeric(st_area(pols_ready))) / 10000
total_raster_val   <- sum(global(final_stack, "sum", na.rm=TRUE)$sum)
total_raster_ha    <- total_raster_val / SCALE_FACTOR

diff_pct <- abs(total_poly_area_ha - total_raster_ha) / total_poly_area_ha * 100
message(sprintf("Polygon Area: %.2f ha | Raster Area: %.2f ha | Diff: %.4f%%", 
                total_poly_area_ha, total_raster_ha, diff_pct))

# -----------------------------------------------------------------------------
# 7. Write Output
# -----------------------------------------------------------------------------
OUTPUT_TIF <- "./scripts/mining_biome_area_int.tif"
message("Writing raster to: ", OUTPUT_TIF)

writeRaster(final_stack, OUTPUT_TIF, 
            overwrite = TRUE, 
            datatype = DATA_TYPE,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2"))

# -----------------------------------------------------------------------------
# 8. Metadata
# -----------------------------------------------------------------------------
OUTPUT_META <- "./scripts/mining_biome_area_metadata.txt"
message("Writing metadata to: ", OUTPUT_META)

info_date <- Sys.time()
info_layers <- names(final_stack)

metadata_content <- c(
  "===============================================================================",
  " METADATA: MINING AREA PER BIOME RASTER (50KM)",
  "===============================================================================",
  "",
  "1. GENERAL INFORMATION",
  "----------------------",
  paste0("Filename:        ", basename(OUTPUT_TIF)),
  paste0("Source Data:     ", INPUT_GPKG),
  paste0("Biome Source:    Ecoregions2017 (Resolve Ecoregions)"),
  paste0("Date Generated:  ", info_date),
  paste0("Data Type:       ", DATA_TYPE, " (Signed 32-bit Integer)"),
  "",
  "2. DATA INTERPRETATION",
  "----------------------",
  "Unit:               Hectares (ha)",
  paste0("Scale Factor:       ", SCALE_FACTOR),
  "Precision:          0.01 ha",
  "",
  "*** HOW TO READ VALUES ***",
  paste0("Area (ha) = Raster_Value / ", SCALE_FACTOR),
  "",
  "3. VALIDATION",
  "-------------",
  paste0("Total Polygon Area: ", sprintf("%.2f ha", total_poly_area_ha)),
  paste0("Total Raster Area:  ", sprintf("%.2f ha", total_raster_ha)),
  "",
  "4. LAYER LIST",
  "-------------"
)

for (i in seq_along(info_layers)) {
  metadata_content <- c(metadata_content, paste0("Band ", i, ": ", info_layers[i]))
}

writeLines(metadata_content, OUTPUT_META)

# -----------------------------------------------------------------------------
# 9. Rank Check
# -----------------------------------------------------------------------------
message("\n--- Ranking of Mining Area per Biome (Derived from Raster) ---")

# Sum all pixels per layer
layer_sums <- global(final_stack, "sum", na.rm = TRUE)

# Create summary dataframe
biome_rank <- data.frame(
  biome_layer = names(final_stack),
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
  select(Biome = biome_layer, Area_Ha = area_ha, Percent = percentage)

print(biome_rank, row.names = FALSE)

message("Done.")