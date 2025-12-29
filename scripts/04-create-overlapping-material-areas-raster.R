# -----------------------------------------------------------------------------
# Script: calculate_overlapping_material_areas.R
# Optimized for high-speed Cloud Optimized GeoTIFF (COG) serving
# Units: Square Kilometers (km2) scaled by 10 to fit INT2U
# -----------------------------------------------------------------------------

library(sf)
library(terra)
library(dplyr)
library(stringr)
library(tidyr)

# 1. USER CONFIGURATION
DATA_VERSION <- "20251030-all_materials" 
INPUT_FILE   <- file.path("output", DATA_VERSION, "cluster_features.gpkg")
N_TOP        <- 5       # Number of top materials to process

TARGET_RES   <- 0.5     # Final resolution in DEGREES
FINE_RES     <- 0.1     # High precision sub-grid
# SCALE_FACTOR 10 preserves 1 decimal place (e.g., 1.23 km2 -> 12)
# Max value 6553.5 km2 fits in INT2U. 0.5deg cell is ~3000 km2 max.
SCALE_FACTOR <- 10      
DATA_TYPE    <- "INT2U" # Unsigned 16-bit (0-65535)
NA_FLAG      <- 65535   # Standard NoData for INT2U

MATERIAL_COLUMNS <- c("materials_list")

# 2. LOAD AND PRE-PROCESS DATA
message("Loading data from: ", INPUT_FILE)
if (!file.exists(INPUT_FILE)) stop("Input file not found.")

pols_raw <- st_read(INPUT_FILE, quiet = TRUE)

if (!"id" %in% names(pols_raw)) {
  message("Warning: 'id' column not found. Creating temporary IDs.")
  pols_raw$id <- seq_len(nrow(pols_raw))
}

pols_subset <- pols_raw |> filter(str_starts(as.character(id), "A"))

pols_expanded_attr <- pols_subset |>
  st_drop_geometry() |>
  select(id, any_of(MATERIAL_COLUMNS)) |>
  unite("combined_materials", any_of(MATERIAL_COLUMNS), sep = ",", remove = TRUE, na.rm = TRUE) |>
  separate_rows(combined_materials, sep = ",\\s*") |>
  rename(material = combined_materials) |>
  mutate(material = str_trim(material)) |>
  filter(material != "" & material != "NA") |>
  distinct(id, material)

# Keep area_mine for summary but use spatial geometry for rasterization
pols_expanded <- pols_subset |> select(id, area_mine) |> inner_join(pols_expanded_attr, by = "id")

mat_summary <- st_drop_geometry(pols_expanded) |>
  group_by(material) |>
  summarise(total_area_mine = sum(area_mine, na.rm = TRUE), n_polygons = n()) |>
  arrange(desc(total_area_mine)) |>
  mutate(pct_area = total_area_mine / sum(total_area_mine, na.rm = TRUE) * 100) |>
  slice_max(n = N_TOP, order_by = total_area_mine)

print(mat_summary, n = N_TOP)

# 3. DEFINE GRIDS
# Using km2 as the base unit to ensure values fit in 16-bit integer
r_fine_template <- rast(ext(pols_raw), resolution = FINE_RES, crs = crs(pols_raw))
cell_areas_fine_km2 <- cellSize(r_fine_template, unit = "km") 
agg_factor <- round(TARGET_RES / FINE_RES)

# 4. CREATE MULTI-LAYER RASTER & VALIDATE
raster_layers <- list()

for (mat in mat_summary$material) {
  mat_subset <- pols_expanded |> filter(material == mat)
  if (nrow(mat_subset) > 0) {
    # A. VECTOR CALCULATION (Exact Truth in km2)
    vec_area_km2 <- sum(as.numeric(st_area(mat_subset))) / 1e6
    
    # B. RASTER CALCULATION
    r_fine_cov <- rasterize(mat_subset, r_fine_template, cover = TRUE, background = 0)
    r_fine_area_km2 <- r_fine_cov * cell_areas_fine_km2
    r_coarse_area_km2 <- terra::aggregate(r_fine_area_km2, fact = agg_factor, fun = "sum", na.rm = TRUE)
    
    # C. SCALE AND VALIDATE
    # Scale factor 10 fits large 0.5 deg pixels into 16-bit (max ~6553 km2)
    r_int <- round(r_coarse_area_km2 * SCALE_FACTOR)
    
    # Validation
    rst_sum_int <- global(r_int, "sum", na.rm = TRUE)$sum
    rst_area_km2 <- rst_sum_int / SCALE_FACTOR
    
    diff_pct <- abs(vec_area_km2 - rst_area_km2) / vec_area_km2 * 100
    if (is.nan(diff_pct) || is.infinite(diff_pct)) diff_pct <- 0
    
    message(sprintf("%d/%d: %-15s | Vector: %9.2f km2 | Raster: %9.2f km2 | Diff: %.3f%%", 
                    which(mat_summary$material==mat), length(mat_summary$material), substr(mat, 1, 15), vec_area_km2, rst_area_km2, diff_pct))
    
    # D. OPTIMIZATION - Set 0 to NA
    r_int[r_int <= 0] <- NA
    
    safe_name <- str_replace_all(mat, "[^[:alnum:]]", "_")
    names(r_int) <- safe_name
    raster_layers[[safe_name]] <- r_int
  } 
}

final_stack <- rast(raster_layers)

# 5. WRITE OUTPUT (COG-OPTIMIZED)
OUTPUT_TIF <- "scripts/overlapping_material_areas_km2_scaled.tif"

message("\nWriting COG-optimized raster (km2 scaled by 10)...")

writeRaster(final_stack, OUTPUT_TIF, 
            filetype = "COG", 
            datatype = DATA_TYPE, 
            NAflag = NA_FLAG,
            gdal = c(
              "COMPRESS=DEFLATE", 
              "LEVEL=9",
              "PREDICTOR=2",
              "TILED=YES",
              "OVERVIEWS=YES",
              "RESAMPLING=BILINEAR"
            ),
            overwrite = TRUE)

message("Optimized COG successfully generated at: ", OUTPUT_TIF)