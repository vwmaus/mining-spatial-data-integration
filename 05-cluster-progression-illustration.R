# -----------------------------------------------------------------
# SCRIPT: 05-plot-cluster-progression.R
# -----------------------------------------------------------------
#
# This script generates a multi-panel plot to visualize the hierarchical
# clustering process for a specific geographic area (e.g., a region in Ghana).
#
# It loads the intermediate clustering results and plots the same set of
# features at different distance thresholds (e.g., 1km, 3km, 5km, 7km).
# This provides a clear visual illustration of how small, separate
# features gradually merge into larger clusters as the spatial
# search distance increases.
#
################################################################################

library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(patchwork)


# Define the clustering thresholds (in km) to visualize
h_levels_to_plot <- c(1, 3, 5, 7)

# Define the output file path
output_dir <- "./output/20251030-all_materials"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE) # Added recursive
output_filename <- file.path(output_dir, "plot_cluster_progression_ghana.png")

# --- Data Loading ---
cat("Loading required data...\n")

# Load the base feature geometries
tryCatch({
  cluster_data <- st_read("./tmp/cluster_data.gpkg", quiet = TRUE)
}, error = function(e) {
  stop("Error: Cannot find './tmp/cluster_data.gpkg'. Please run script 00 first.", call. = FALSE)
})

# Load the hierarchical clustering results (links features to cluster IDs at each threshold)
tryCatch({
  # --- MODIFIED: Select all columns needed for plotting (IDs and primary commodity lists) ---
  h_id_cols <- str_c("id_hc_", h_levels_to_plot)
  h_primary_cols <- str_c("primary_hc_", h_levels_to_plot)
  
  hcluster_concordance <- read_csv("./tmp/hcluster_concordance.csv", 
                                 # Select only the columns needed
                                 col_select = c("id", all_of(h_id_cols), all_of(h_primary_cols)),
                                 show_col_types = FALSE)
}, error = function(e) {
  stop("Error: Cannot find './tmp/hcluster_concordance.csv'. Or it is missing required 'primary_hc_' columns. Please run script 01 first.", call. = FALSE)
})

# --- Data Preparation ---
cat("Preparing data for plotting...\n")

# Define the spatial extent to plot (Bounding Box)
#    Estimated from the example image 'cluster-ghana-01.jpg'
#    (Y: ~5°12'N to 5°24'N, X: ~2°12'W to 1°48'W)
target_bbox <- st_bbox(c(xmin = -2.3, ymin = 5.2, xmax = -1.7, ymax = 5.8), crs = 4326)

# Filter data to the target bounding box
#    We use the cluster_data 'id' as the main filter
# --- MODIFIED: Join area_mine and hcluster data to features in bbox ---
base_data_in_bbox <- cluster_data |>
  st_filter(st_as_sfc(target_bbox)) |>
  select(id, area_mine, geom) |>
  # join the cluster IDs and primary commodity info
  left_join(hcluster_concordance, by = "id")

if (nrow(base_data_in_bbox) == 0) {
  stop("Error: No features found within the specified target_bbox. Check coordinates.", call. = FALSE)
}

# 3. Create the list of columns to select (IDs and primary lists)
h_cols_to_select <- c(str_c("id_hc_", h_levels_to_plot), str_c("primary_hc_", h_levels_to_plot))

# 4. Prepare the dataset for plotting
plot_data_long <- base_data_in_bbox |>
  st_drop_geometry() |>
  # Pivot from "wide" to "long" format
  pivot_longer(
    cols = all_of(h_cols_to_select),
    # --- MODIFIED: Use names_pattern to auto-separate id and primary columns ---
    names_to = c(".value", "threshold_km"), # ".value" auto-matches "id_hc_" and "primary_hc_"
    names_pattern = "(id_hc|primary_hc)_(\\d+)"
  ) |>
  # Filter again to ensure we only have the exact numeric levels we want
  filter(as.numeric(threshold_km) %in% h_levels_to_plot) |>
  mutate(
    threshold_label = factor(threshold_km, levels = h_levels_to_plot) |>
                      paste0(" km Threshold"),
    cluster_id = as.factor(id_hc), # Treat cluster ID as a discrete color
    area_mine = as.numeric(area_mine) # ensure area is numeric
  ) |>
  # Join back with the geometries
  left_join(select(base_data_in_bbox, id, geom), by = "id") |>
  st_as_sf()

# --- NEW: Calculate Error Statistics for Text Boxes ---
cat("Calculating error statistics for plot...\n")

error_stats <- plot_data_long |>
  st_drop_geometry() |>
  group_by(threshold_label) |>
  summarise(
    total_area = sum(area_mine, na.rm = TRUE),
    unassigned_area = sum(area_mine[is.na(primary_hc)], na.rm = TRUE),
    # A multi-primary assignment is one that contains a comma
    multi_assigned_area = sum(area_mine[str_detect(primary_hc, ",")], na.rm = TRUE)
  ) |>
  mutate(
    pct_unassigned = unassigned_area / total_area,
    pct_multi = multi_assigned_area / total_area,
    # Create the text label
    label = sprintf("Unassigned: %.1f%%\nMulti-primary: %.1f%%", 
                    pct_unassigned * 100, 
                    pct_multi * 100)
  ) |>
  # Set coordinates for the text box (top left corner)
  mutate(
    x_pos = st_bbox(target_bbox)["xmin"],
    y_pos = st_bbox(target_bbox)["ymax"]
  )


# --- Plotting ---
cat("Generating multi-panel plot...\n")

# --- NEW: Use a reproducible, high-contrast color palette ---
set.seed(123) # for reproducible color shuffling
unique_ids <- levels(plot_data_long$cluster_id)
# Use 'Dynamic' palette for max distinction, shuffle them
color_palette <- sample(hcl.colors(length(unique_ids), palette = "Dynamic"))
names(color_palette) <- unique_ids


gp <- ggplot(plot_data_long) +
  # Plot the features, colored by their cluster ID
  geom_sf(
    aes(fill = cluster_id, color = cluster_id),
    linewidth = 0.1 # Small border for polygons
  ) +
  coord_sf() +
  # Use the high-contrast color palette
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = color_palette) +
  
  # --- NEW: Add the error statistics text box ---
  geom_text(
    data = error_stats,
    aes(x = x_pos, y = y_pos, label = label),
    hjust = 0, vjust = 1, # Anchor to top-left
    size = 5, 
    color = "black",
    # Add a white background for readability
    label.padding = unit(0.15, "lines"),
    label.r = unit(0, "lines"), # no rounded corners
    label.size = 0, # no border
    nudge_x = 0.35, nudge_y = -0.15 # Small offset from corner
  ) +

  # Create the multi-panel grid, one for each threshold
  facet_wrap(
    ~threshold_label,
    ncol = 2 # Arrange in a 2x2 grid
  ) +
  # Zoom the map to our target bounding box
  coord_sf(
    xlim = c(st_bbox(target_bbox)["xmin"], st_bbox(target_bbox)["xmax"]),
    ylim = c(st_bbox(target_bbox)["ymin"], st_bbox(target_bbox)["ymax"]),
    expand = FALSE
  ) +
  # Use a clean, minimal theme
  theme_void() +
  theme(
    axis.text.x = element_text(), 
    axis.text.y = element_text(),
    panel.grid.major = element_line(color = "grey", linetype = "dashed"),
    legend.position = "none", # Remove legend as requested
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 5)),
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1) # Use linewidth
  )

# --- Save Output ---
ggsave(
  output_filename,
  plot = gp,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

cat("Plot saved successfully to:", output_filename, "\n")
