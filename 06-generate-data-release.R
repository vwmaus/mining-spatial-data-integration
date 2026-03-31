# -----------------------------------------------------------------------------
# Script: calculate_overlapping_material_areas.R
# Optimized for high-speed Cloud Optimized GeoTIFF (COG) serving
# Units: Square Kilometers (km2) scaled by 10 to fit INT2U (16-bit)
# -----------------------------------------------------------------------------

library(sf)
library(terra)
library(dplyr)
library(stringr)
library(tidyr)

# 1. USER CONFIGURATION
IN_DATA_VERSION <- "20260325-all_materials"
OUT_DATA_VERSION <- "20260330"
INPUT_FILE   <- file.path("output", IN_DATA_VERSION, "cluster_features.gpkg")
N_TOP        <- 100     # Number of top materials to process

TARGET_RES   <- 0.5     # Final resolution in DEGREES
FINE_RES     <- 0.1     # High precision sub-grid
SCALE_FACTOR <- 10      # Preserves 1 decimal place (Value / 10 = km2)
DATA_TYPE    <- "INT2U" # Unsigned 16-bit (0-65535)
NA_FLAG      <- 65535   # Standard NoData for INT2U

MATERIAL_COLUMNS <- c("materials_list")
CITATION <- "Maus, V., et al. (2025). An open database on global coal and metal mine production. SSRN. https://dx.doi.org/10.2139/ssrn.5408302"

# 2. LOAD AND PRE-PROCESS DATA
message("Loading data from: ", INPUT_FILE)
if (!file.exists(INPUT_FILE)) stop("Input file not found.")

pols_raw <- st_read(INPUT_FILE, quiet = TRUE)

# Robust ID Check
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

pols_expanded <- pols_subset |> select(id, area_mine) |> inner_join(pols_expanded_attr, by = "id")

mat_summary <- st_drop_geometry(pols_expanded) |>
  group_by(material) |>
  summarise(total_area_mine = sum(area_mine, na.rm = TRUE), n_polygons = n()) |>
  arrange(desc(total_area_mine)) |>
  mutate(pct_area = total_area_mine / sum(total_area_mine, na.rm = TRUE) * 100) |>
  slice_max(n = N_TOP, order_by = total_area_mine)

print(mat_summary)

# 3. DEFINE GRIDS
r_fine_template <- rast(ext(pols_raw), resolution = FINE_RES, crs = crs(pols_raw))
cell_areas_fine_km2 <- cellSize(r_fine_template, unit = "km") 
agg_factor <- round(TARGET_RES / FINE_RES)

# 4. CREATE MULTI-LAYER RASTER
raster_layers <- list()

for (mat in mat_summary$material) {
  mat_subset <- pols_expanded |> filter(material == mat)
  if (nrow(mat_subset) > 0) {
    # A. VECTOR CALCULATION (Exact Truth in km2)
    vec_area_km2 <- sum(as.numeric(st_area(mat_subset))) / 1e6
    
    # B. RASTER CALCULATION
    r_fine_cov <- rasterize(mat_subset, r_fine_template, cover = TRUE, background = 0)
    r_fine_area_km2 <- r_fine_cov * cell_areas_fine_km2
    r_coarse_area_km2 <- aggregate(r_fine_area_km2, fact = agg_factor, fun = "sum", na.rm = TRUE)
    
    # C. SCALE
    r_int <- round(r_coarse_area_km2 * SCALE_FACTOR)
    
    # Set 0 to NA for transparency/optimization
    r_int[r_int <= 0] <- NA
    
    safe_name <- str_replace_all(mat, "[^[:alnum:]]", "_")
    names(r_int) <- safe_name
    raster_layers[[safe_name]] <- r_int
    
    # Validation message
    rst_area_km2 <- global(r_int, "sum", na.rm = TRUE)$sum / SCALE_FACTOR
    diff_pct <- abs(vec_area_km2 - rst_area_km2) / vec_area_km2 * 100
    message(sprintf("%d/%d: %-15s | Vector: %9.2f km2 | Raster: %9.2f km2 | Diff: %.3f%%",
    which(mat_summary$material==mat), length(mat_summary$material), substr(mat, 1, 15), vec_area_km2, rst_area_km2, diff_pct))
  } 
}

final_stack <- rast(raster_layers)

# 5. METADATA ENRICHMENT
# Set units and descriptions for the raster object
units(final_stack) <- "km2 * 10"
# Add global metadata tags that will be written to the COG
metags(final_stack) <- c(
  TITLE = "Global Mining Area By Material",
  DESCRIPTION = "Estimated areas in km2 scaled by 10. Formula: Area_km2 = Pixel_Value / 10.",
  SOURCE = CITATION,
  DATA_VERSION = OUT_DATA_VERSION,
  CREATION_DATE = as.character(Sys.Date())
)

# 6. WRITE OUTPUT (COG-OPTIMIZED)
OUTPUT_DIR <- file.path("output", IN_DATA_VERSION, "data_release")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)
OUTPUT_TIF <- file.path(OUTPUT_DIR, str_c("material_areas_km2-", OUT_DATA_VERSION ,".tif"))
README_FILE <- file.path(OUTPUT_DIR, str_c("material_areas_km2-", OUT_DATA_VERSION ,".txt"))

message("\nWriting COG-optimized raster...")
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

# 7. GENERATE AUTOMATED README
readme_content <- c(
  "===============================================================",
  "DATASET: Global Mining Area By Material",
  "===============================================================",
  "",
  "OVERVIEW:",
  "This Cloud Optimized GeoTIFF (COG) contains estimated",
  "mining areas for specific materials.",
  "",
  "CITATION & SOURCE:",
  paste("Article:", CITATION),
  "",
  "TECHNICAL SPECIFICATIONS:",
  paste("- Resolution:     ", TARGET_RES, "degrees"),
  paste("- Coordinate Sys: ", crs(final_stack, proj = TRUE)),
  paste("- Data Type:      ", DATA_TYPE, "(16-bit Unsigned Integer)"),
  paste("- NoData Value:   ", NA_FLAG),
  "",
  "SCALING & UNITS:",
  "IMPORTANT: Values are stored as integers to minimize file size.",
  "Units are Square Kilometers (km2) scaled by a factor of 10.",
  "",
  "Formula: Actual Area (km2) = Pixel Value / 10",
  "",
  "INCLUDED LAYERS (Materials):",
  paste("-", names(final_stack), collapse = "\n"),
  "",
  paste("Data Source Version:", IN_DATA_VERSION),
  paste("Data Source Version:", OUT_DATA_VERSION),
  paste("Generated on:", Sys.Date()),
  "==============================================================="
)

writeLines(readme_content, README_FILE)
message("Metadata-enriched COG and README.txt generated in: ", OUTPUT_DIR)
