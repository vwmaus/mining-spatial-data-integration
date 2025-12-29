# -----------------------------------------------------------------------------
# Script: calculate_selected_materials_area.R
# Location: ./scripts/
# Description: 
#   1. Reads mining spatial data (cluster_features.gpkg).
#   2. Filters polygons that contain ANY of the selected materials 
#      (Searching in Primary AND Byproduct columns).
#   3. METHODOLOGY:
#      - Rasterizes at a FINE resolution (0.5 deg) first to calculate accurate areas.
#      - Aggregates (sums) this fine raster to the COARSE target grid (5.0 deg).
#   4. Saves as a single-layer 32-bit INTEGER raster (.tif).
#   5. Generates detailed metadata.
# -----------------------------------------------------------------------------

library(sf)
library(terra)
library(dplyr)
library(stringr)

# Create output directory
dir.create("./scripts", showWarnings = FALSE)

# =============================================================================
# 1. USER CONFIGURATION
# =============================================================================

# Define the materials to select (Case insensitive)
SELECTED_MATERIALS <- c("Gold", "Nickel", "Copper")

# Define Target Grid Resolution (Degrees)
TARGET_RES_DEG <- 0.5

# Define Intermediate Fine Resolution (Degrees)
# Calculating at 0.5 deg first reduces area distortion errors before aggregating.
# Note: Since Target is 0.5, Fine should ideally be smaller (e.g., 0.05 or 0.1) 
# OR if Target is 5.0, Fine 0.5 is good.
# Assuming previous request context: Target 0.5 deg.
# If Target is 0.5, we don't strictly *need* a finer step unless precision is critical,
# but keeping the 2-step logic is safer. Let's set Fine to 0.05 for 0.5 target.
# HOWEVER, to keep it simple and fast based on your prompt's structure, 
# if Target is 0.5, we can just use 0.5 directly. 
# But let's stick to the prompt variables. 
FINE_RES_DEG <- 0.5 

# Define which columns to search for materials
# "list_commodities" usually includes Primary + Byproducts
SEARCH_COLUMNS <- c("primary_materials_list", "list_commodities")

# Output settings
# Scale Factor 10 means: Integer 1 = 0.1 Hectares.
SCALE_FACTOR  <- 10 
DATA_TYPE     <- "INT4S" # Signed 32-bit integer

# Input Data Path
DATA_VERSION <- "20251030-all_materials" 
INPUT_FILE   <- file.path("output", DATA_VERSION, "cluster_features.gpkg")

# =============================================================================
# 2. LOAD AND FILTER DATA
# =============================================================================
message("Loading data from: ", INPUT_FILE)

if (!file.exists(INPUT_FILE)) {
  stop("Input file not found. Please ensure '", INPUT_FILE, "' exists.")
}

# 1. Read Polygons (ID starts with 'A')
pols_raw <- st_read(INPUT_FILE, quiet = TRUE) %>%
  filter(str_starts(id, "A"))

message(paste("Total polygons loaded:", nrow(pols_raw)))

# 2. Check search columns
existing_cols <- intersect(SEARCH_COLUMNS, names(pols_raw))

if (length(existing_cols) == 0) {
  stop(paste("None of the specified search columns found. Available:", 
             paste(names(pols_raw), collapse=", ")))
}

message(paste("Searching in columns:", paste(existing_cols, collapse=", ")))

# 3. Filter Polygons
filter_pattern <- paste(SELECTED_MATERIALS, collapse = "|")
message(paste("Filtering for materials matching:", filter_pattern))

# Use if_any to check if the pattern exists in ANY of the existing columns
pols_selected <- pols_raw %>%
  filter(if_any(all_of(existing_cols), ~ str_detect(., regex(filter_pattern, ignore_case = TRUE))))

num_selected <- nrow(pols_selected)
message(paste("Polygons selected:", num_selected))

if (num_selected == 0) {
  stop("No polygons matched the selected materials.")
}

# =============================================================================
# 3. RASTERIZE (FINE) AND AGGREGATE (COARSE)
# =============================================================================
message("--- Starting Rasterization ---")

# Step 1: Define Grid
# If Fine == Target, we skip aggregation step effectively.
message(paste("1. Rasterizing at fine resolution:", FINE_RES_DEG, "degrees..."))
r_fine_template <- rast(ext(pols_raw), resolution = FINE_RES_DEG, crs = crs(pols_raw))

# Calculate cell area for fine grid (Hectares)
cell_areas_fine_ha <- cellSize(r_fine_template, unit = "km") * 100

# Rasterize Fine: Fraction of coverage
r_fine_fraction <- rasterize(pols_selected, r_fine_template, cover = TRUE, background = 0)

# Calculate Area in Hectares (Fine)
r_fine_area_ha <- r_fine_fraction * cell_areas_fine_ha

# Step 2: Aggregate to Target Grid
if (TARGET_RES_DEG > FINE_RES_DEG) {
  message(paste("2. Aggregating to target resolution:", TARGET_RES_DEG, "degrees..."))
  agg_factor <- round(TARGET_RES_DEG / FINE_RES_DEG)
  r_coarse_area_ha <- terra::aggregate(r_fine_area_ha, fact = agg_factor, fun = "sum", na.rm = TRUE)
  r_final_ha <- r_coarse_area_ha
} else {
  message(paste("Target resolution matches fine resolution. Skipping aggregation."))
  r_final_ha <- r_fine_area_ha
}

# Apply Scale Factor and Round
r_int <- round(r_final_ha * SCALE_FACTOR)

# Name the layer
layer_name <- paste0("Area_", paste(substr(SELECTED_MATERIALS, 1, 3), collapse=""))
names(r_int) <- layer_name

# =============================================================================
# 4. VALIDATION
# =============================================================================
message("Performing validation...")

# Total area of selected polygons (Geometric area)
total_poly_area_ha <- sum(as.numeric(st_area(pols_selected))) / 10000

# Total area in Raster
total_raster_val <- sum(global(r_int, "sum", na.rm = TRUE)$sum)
total_raster_ha  <- total_raster_val / SCALE_FACTOR

diff_pct <- abs(total_poly_area_ha - total_raster_ha) / total_poly_area_ha * 100

message(sprintf("Selected Poly Area: %.2f ha", total_poly_area_ha))
message(sprintf("Raster Aggregated:  %.2f ha", total_raster_ha))
message(sprintf("Difference:         %.4f%%", diff_pct))

# =============================================================================
# 5. WRITE OUTPUT
# =============================================================================
OUTPUT_TIF <- "./scripts/selected_materials_area_int.tif"
message(paste("Writing raster to:", OUTPUT_TIF))

writeRaster(r_int, OUTPUT_TIF, 
            overwrite = TRUE, 
            datatype = DATA_TYPE,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2"))

# =============================================================================
# 6. GENERATE METADATA
# =============================================================================
OUTPUT_META <- "./scripts/selected_materials_area_metadata.txt"
message(paste("Writing metadata to:", OUTPUT_META))

info_date <- Sys.time()
info_crs <- if (!is.na(crs(r_int, describe=TRUE)$name)) crs(r_int, describe=TRUE)$name else "EPSG:4326"

metadata_content <- c(
  "===============================================================================",
  " METADATA: SELECTED MINING MATERIALS AREA RASTER",
  "===============================================================================",
  "",
  "1. GENERAL INFORMATION",
  "----------------------",
  paste0("Filename:        ", basename(OUTPUT_TIF)),
  paste0("Date Generated:  ", info_date),
  paste0("Target Grid:     ", TARGET_RES_DEG, " degrees"),
  paste0("Process Grid:    ", FINE_RES_DEG, " degrees"),
  paste0("Data Type:       ", DATA_TYPE, " (Signed 32-bit Integer)"),
  "",
  "2. SELECTION CRITERIA",
  "---------------------",
  paste0("Selected Materials: ", paste(SELECTED_MATERIALS, collapse = ", ")),
  paste0("Search Columns:     ", paste(existing_cols, collapse = ", ")),
  paste0("Polygons Found:     ", num_selected),
  "Logic: Polygons included if ANY search column contains ANY selected material.",
  "",
  "3. DATA INTERPRETATION",
  "----------------------",
  "Unit:               Hectares (ha)",
  paste0("Scale Factor:       ", SCALE_FACTOR),
  "Precision:          0.1 ha",
  "",
  "*** HOW TO READ VALUES ***",
  paste0("To get the area in Hectares, DIVIDE the raster value by ", SCALE_FACTOR, "."),
  paste0("  Formula: Area (ha) = Raster_Value / ", SCALE_FACTOR),
  "",
  "4. VALIDATION",
  "-------------",
  paste0("Total Polygon Area: ", sprintf("%.2f ha", total_poly_area_ha)),
  paste0("Total Raster Area:  ", sprintf("%.2f ha", total_raster_ha)),
  paste0("Difference:         ", sprintf("%.4f%%", diff_pct)),
  "",
  "5. SPATIAL REFERENCE",
  "--------------------",
  paste0("Coordinate System: ", info_crs),
  paste0("Resolution:        ", paste(res(r_int), collapse = " x "), " (deg)")
)

writeLines(metadata_content, OUTPUT_META)
message("Done.")