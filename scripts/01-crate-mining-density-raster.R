# -----------------------------------------------------------------------------
# Script: calculate_mining_density.R
# Location: ./scripts/
# Description: 
#   1. Reads the multi-layer mining area raster (mining_materials_area_int.tif).
#   2. Sums all commodity layers to get total mining area.
#   3. Calculates pixel coverage density (0-100%).
#   4. Applies a scale factor (x100) to preserve 2 decimal places of precision.
#   5. Saves as a single-layer 16-bit INTEGER raster (INT2S).
#   6. Generates metadata.
# -----------------------------------------------------------------------------

library(terra)

# -----------------------------------------------------------------------------
# 1. Configuration
# -----------------------------------------------------------------------------
INPUT_FILE <- "./scripts/mining_materials_area_int.tif"
OUTPUT_TIF <- "./scripts/mining_density_int.tif"
OUTPUT_META <- "./scripts/mining_density_metadata.txt"

# Input configuration (Must match the previous script's settings)
INPUT_SCALE_FACTOR <- 100  # Input values are (Hectares * 100)

# Output configuration
OUTPUT_SCALE_FACTOR <- 100 # Output values will be (Percentage * 100)
OUTPUT_DATATYPE     <- "INT2S" # Signed 16-bit Integer (Max 32,767 covers 100.00%)

# -----------------------------------------------------------------------------
# 2. Load and Process
# -----------------------------------------------------------------------------
message("Loading input raster: ", INPUT_FILE)

if (!file.exists(INPUT_FILE)) {
  stop("Input file not found. Please run 'calculate_material_areas.R' first.")
}

r_stack <- rast(INPUT_FILE)

message("Calculating total mining area...")
# Sum all material layers to get total mining area per pixel
# Current Unit: Scaled Hectares (Ha * 100)
r_total_scaled <- sum(r_stack, na.rm = TRUE)

message("Calculating cell areas...")
# Calculate area of each pixel in Hectares
# unit="km" gives km^2. Multiply by 100 to get Ha.
r_cell_area_ha <- cellSize(r_stack, unit = "km") * 100

message("Computing density percentage...")

# Formula Logic:
# 1. Retrieve Real Area (Ha) = Input_Value / INPUT_SCALE_FACTOR
# 2. Calculate Fraction      = Real_Area / Cell_Area_Ha
# 3. Calculate Percentage    = Fraction * 100
# 4. Scale for Storage       = Percentage * OUTPUT_SCALE_FACTOR

# Combined Formula:
# Output = ( (Input / 100) / CellArea ) * 100 * 100
# Output = ( Input / CellArea ) * 100

r_density_scaled <- (r_total_scaled / r_cell_area_ha) * OUTPUT_SCALE_FACTOR

# Round to integer
r_final <- round(r_density_scaled)

# Validation: Clamp values to max 100% (Scale 10000)
# Floating point arithmetic or polygon precision might cause tiny overflows > 100%
r_final <- clamp(r_final, lower = 0, upper = 10000)

# Set layer name
names(r_final) <- "mining_density_pct"

# -----------------------------------------------------------------------------
# 3. Write Output
# -----------------------------------------------------------------------------
message(paste("Writing density raster to:", OUTPUT_TIF))

writeRaster(r_final, OUTPUT_TIF, 
            overwrite = TRUE, 
            datatype = OUTPUT_DATATYPE,
            gdal = c("COMPRESS=LZW", "PREDICTOR=2"))

# -----------------------------------------------------------------------------
# 4. Generate Metadata
# -----------------------------------------------------------------------------
message(paste("Writing metadata to:", OUTPUT_META))

info_date <- Sys.time()
info_crs <- if (!is.na(crs(r_final, describe=TRUE)$name)) crs(r_final, describe=TRUE)$name else "EPSG:4326"
info_res <- paste(res(r_final), collapse = " x ")

# Calculate quick stats for metadata
stats <- global(r_final, c("min", "max", "mean"), na.rm=TRUE)

metadata_content <- c(
  "===============================================================================",
  " METADATA: MINING DENSITY RASTER",
  "===============================================================================",
  "",
  "1. GENERAL INFORMATION",
  "----------------------",
  paste0("Filename:        ", basename(OUTPUT_TIF)),
  paste0("Input Source:    ", basename(INPUT_FILE)),
  paste0("Date Generated:  ", info_date),
  paste0("Data Type:       ", OUTPUT_DATATYPE, " (Signed 16-bit Integer)"),
  "",
  "2. DATA INTERPRETATION (CRITICAL)",
  "---------------------------------",
  "Content:            Percentage of pixel area covered by mining activities",
  paste0("Scale Factor:       ", OUTPUT_SCALE_FACTOR),
  "Precision:          0.01 %",
  "Range:              0 to 10000 (representing 0% to 100%)",
  "",
  "*** HOW TO READ VALUES ***",
  paste0("To get the percentage, DIVIDE the raster value by ", OUTPUT_SCALE_FACTOR, "."),
  paste0("  Formula: Density (%) = Raster_Value / ", OUTPUT_SCALE_FACTOR),
  paste0("  Example: A value of 1550 means 15.50% of the pixel is covered by mining."),
  "",
  "3. SPATIAL REFERENCE",
  "--------------------",
  paste0("Coordinate System: ", info_crs),
  paste0("Resolution:        ", info_res),
  "",
  "4. STATISTICS (Scaled Values)",
  "-----------------------------",
  paste0("Min Value:         ", stats$min),
  paste0("Max Value:         ", stats$max, " (Should be <= 10000)"),
  paste0("Mean Value:        ", round(stats$mean, 2)),
  "",
  "5. METHODOLOGY",
  "--------------",
  "1. Summed all material layers from the input raster (total mining area).",
  "2. Calculated the geographic area of each grid cell (hectares).",
  "3. Computed ratio: (Total Mining Area / Cell Area).",
  "4. Converted to Percentage (0-100) and scaled by 100 for integer storage.",
  ""
)

writeLines(metadata_content, OUTPUT_META)
message("Done.")
