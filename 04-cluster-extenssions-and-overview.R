# -----------------------------------------------------------------
# SCRIPT: 04-cluster-extenssions-and-overview.R
# -----------------------------------------------------------------
#
# This script is the final post-processing and figure-generation step,
# creating many of the key tables and plots for the final manuscript.
#
# Workflow:
# 1.  Reads the final `cluster_features.gpkg` from the data release.
# 2.  Performs geo-enrichment by spatially joining the clusters with
#     country boundaries and ecoregion data.
# 3.  Generates a LaTeX summary table (`tbl-cluster-summary.tex`) that
#     aggregates statistics by "Assigned" vs. "Unassigned" clusters.
# 4.  Generates a LaTeX summary table (`tbl-polygon-source-summary.tex`) that
#     breaks down the results by the original data sources (Maus, Tang, etc.).
# 5.  Generates key manuscript figures, including:
#     - Bar chart of mined area by country and material.
#     - Bar chart of mined area by biome and material.
#     - Treemap of total mined area by primary material.
# 6.  Exports a final supplementary data file (`s02-mine_area_accounting.csv`)
#     with detailed area accounting.

# -----------------------------------------------------------------
# PART 0: SETUP
# -----------------------------------------------------------------

# --- Load Libraries ---
library(sf)
library(dplyr)
library(stringr)
library(progress)
library(readr)
library(tidyr)
library(forcats)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(knitr)
library(kableExtra)
library(scales)
library(treemapify)
library(patchwork) 

# --- Source Helper Functions ---
if(file.exists("./R/s2_union_split_agg.R")) {
  source("./R/s2_union_split_agg.R")
} else {
  warning("s2_union_split_agg.R not found. Map plotting might fail.")
}

# --- Define Parameters ---
data_version <- "20251030-all_materials"
output_dir <- str_c("./output/", data_version)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

top_n_countries <- 15
top_n_materials <- 7

# Disable S2 for joins, enable for aggregation
sf_use_s2(FALSE)

# -----------------------------------------------------------------
# PART 1: LOAD & ENRICH CORE DATA
# -----------------------------------------------------------------
cat("Loading and enriching core data...\n")

# --- Load Base Cluster Data (Spatial) ---
cluster_features_sf <- st_read(str_c(output_dir, "/cluster_features.gpkg"))

# --- Enrich with Country Data ---
country_tbl_path <- str_c(output_dir, "/country_tbl.csv")
if(!file.exists(country_tbl_path)){
  cat("Generating country lookup table...\n")
  country_data <- ne_countries(scale = "medium", returnclass = "sf") |>
    select(country_name = admin, country_isoa3 = adm0_a3)
    
  st_centroid(cluster_features_sf) |>
    st_join(country_data, join = st_nearest_feature) |>
    st_drop_geometry() |>
    as_tibble() |>
    select(id, country_name, country_isoa3) |>
    write_csv(country_tbl_path)
}
country_tbl <- read_csv(country_tbl_path, show_col_types = FALSE)

# --- Enrich with Ecoregion Data ---
ecoregion_tbl_path <- str_c(output_dir, "/ecoregion_tbl.csv")
if(!file.exists(ecoregion_tbl_path)){
  cat("Generating ecoregion lookup table...\n")
  if(!file.exists("./tmp/ecoregions/ecoregions.gpkg")){
    dir.create("./tmp/ecoregions", showWarnings = FALSE, recursive = TRUE)
    download.file("https://storage.googleapis.com/teow2016/Ecoregions2017.zip", destfile = "./tmp/Ecoregions2017.zip")
    unzip("./tmp/Ecoregions2017.zip", exdir = "./tmp/ecoregions/")
    st_read("./tmp/ecoregions/Ecoregions2017.shp") |>
      select(ecoregion_name = ECO_NAME, biome_name = BIOME_NAME) |>
      st_simplify() |>
      st_write("./tmp/ecoregions/ecoregions.gpkg")
  }
  
  st_centroid(cluster_features_sf) |>
    st_join(st_read("./tmp/ecoregions/ecoregions.gpkg"), join = st_nearest_feature) |>
    st_drop_geometry() |>
    as_tibble() |>
    select(id, ecoregion_name, biome_name) |> # Fixed typo 'ecoregions_name'
    write_csv(ecoregion_tbl_path)
}
ecoregion_tbl <- read_csv(ecoregion_tbl_path, show_col_types = FALSE)

# --- Create Master Enriched Tibble (Non-Spatial) ---
cluster_features_tbl <- cluster_features_sf |>
    st_drop_geometry() |>
    as_tibble() |>
    left_join(country_tbl, by = "id") |>
    left_join(ecoregion_tbl, by = "id")

cat("Data loading and enrichment complete.\n")

# -----------------------------------------------------------------
# PART 2: GENERATE MANUSCRIPT TABLES
# -----------------------------------------------------------------
cat("Generating LaTeX summary tables...\n")

# --- Table 1: Cluster Summary (Assigned vs. Unassigned) ---
summary_tbl <- cluster_features_tbl |>
    mutate(area_mine = area_mine * 1e-6) |>
    group_by(id_cluster) |>
    reframe(
        is_assigned = !is.na(unique(primary_materials_list)),
        n_polygons = n_distinct(id[str_starts(id, "A")]),
        avg_area_polygons = mean(area_mine, na.rm = TRUE),
        n_points = n_distinct(id[str_starts(id, "P")]),
        area_cluster = sum(area_mine, na.rm = TRUE),
    ) |>
    group_by(is_assigned) |>
    reframe(
        n_clusters = n_distinct(id_cluster),
        avg_area_polygons = sum(avg_area_polygons * n_polygons, na.rm = TRUE) / sum(n_polygons),
        avg_n_polygons = mean(n_polygons),
        n_polygons = sum(n_polygons),
        avg_n_points = mean(n_points),
        n_points = sum(n_points),
        avg_area_cluster = mean(area_cluster, na.rm = TRUE),
        area_cluster = sum(area_cluster, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
        group = ifelse(is_assigned, "Assigned", "Unassigned"),
        .before = 1
    )

summary_tbl |>
    arrange(desc(area_cluster)) |>
    mutate(
        n_clusters = sprintf("%s (%.1f%%)", comma(n_clusters), 100 * n_clusters / sum(n_clusters)),
        n_polygons = sprintf("%s (%.1f%%)", comma(n_polygons), 100 * n_polygons / sum(n_polygons)),
        n_points = sprintf("%s (%.1f%%)", comma(n_points), 100 * n_points / sum(n_points)),
        area_cluster = sprintf("%s (%.1f%%)", comma(area_cluster, accuracy = 0.1), 100 * area_cluster / sum(area_cluster))
    ) |>
    select(
        `Commodity` = group,
        `Total Area (km²)` = area_cluster,
        `Clusters` = n_clusters,
        `Polygons` = n_polygons,
        `Points` = n_points,
        `Cluster Area (km²)` = avg_area_cluster,        
        `Polygons/Cluster` = avg_n_polygons,
        `Points/Cluster` = avg_n_points,
        `Area/Polygon (km²)` = avg_area_polygons
    ) |>
    kable(
        digits = 1, format = "latex", booktabs = TRUE, linesep = "", align = c("l", rep("r", 8)),
        caption = "Summary of cluster-level features by commodity assignment. \\label{tab:cluster-summary}"
    ) |>
    add_header_above(c(
        " " = 1,
        " " = 1,
        "Number of" = 3,
        "Average" = 4
    )) |>
    kable_styling(latex_options = c("striped", "scale_down", "HOLD_position"), font_size = 12, position = "center") |>
    writeLines(con = file.path(output_dir, "tbl-cluster-summary.tex"))

# --- Table 2: Polygon Source Summary ---
cluster_features_tbl |>
  filter(str_detect(id, "A")) |>
  mutate(
    area_mine = area_mine * 1e-6,
    is_assigned = ifelse(!is.na(primary_materials_list), "Assigned", "Unassigned")
  ) |>
  group_by(is_assigned, data_source) |>
  reframe(
    n_polygons = n_distinct(id),
    area_polygons = sum(area_mine, na.rm = TRUE),
    avg_area_polygons = mean(area_mine, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(is_assigned, desc(area_polygons)) |>
  mutate(
    data_source = str_replace(data_source, fixed("Maus et al. (2022)"), "\\citet{maus_update_2022}"),
    data_source = str_replace(data_source, fixed("Tang & Werner 2023"), "\\citet{tang_global_2023}"),
    data_source = str_replace(data_source, fixed("OpenStreetMap"), "\\citet{openstreetmap_planet_2017}"),
    pct_polygons = 100 * n_polygons / sum(n_polygons),
    pct_area = 100 * area_polygons / sum(area_polygons),
    Polygons = sprintf("%s (%s)", comma(n_polygons), ifelse(pct_polygons < 10,
                                                             sprintf(" %.1f\\%%", pct_polygons),
                                                             sprintf("%.1f\\%%", pct_polygons))),
    avg_Polygons = sprintf("%s", comma(avg_area_polygons, accuracy = 0.1)),
    `Total Area (km²)` = sprintf("%s (%s)", comma(area_polygons, accuracy = 0.1), ifelse(pct_area < 10,
                                                                                          sprintf(" %.1f\\%%", pct_area),
                                                                                          sprintf("%.1f\\%%", pct_area)))
  ) |>
  select(
    `Commodity` = is_assigned,
    `Data Source` = data_source,
    `Total Area (km²)`,
    `Number of Polygons` = Polygons,
    `Average Polygon Area (km²)` = avg_Polygons
  ) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    align = c("l", "l", "r", "r", "r"),
    escape = FALSE,  # allow LaTeX citations to render
    caption = "Polygon counts and areas by commodity assignment and data source.\\label{tab:polygon-source-summary}"
  ) |>
  kable_styling(
    latex_options = c("striped", "scale_down", "hold_position"),
    position = "center"
  ) |>
  writeLines(con = file.path(output_dir, "tbl-polygon-source-summary.tex"))

cat("LaTeX tables generated.\n")

# -----------------------------------------------------------------
# PART 3: PREPARE DATA FOR PLOTTING
# -----------------------------------------------------------------
cat("Preparing base data for plots...\n")

# --- Create expanded material list (key for most plots) ---
material_expanded <- cluster_features_tbl |>
  separate_rows(primary_materials_list, sep = ",\\s*") |>
  group_by(id) |>
  mutate(n_materials = n()) |>
  ungroup() |>
  mutate(
    area_km2 = (area_mine / n_materials) * 1e-6, # allocates area to materials by dividing
    primary_material = ifelse(is.na(primary_materials_list), "Unknown", primary_materials_list)
  )

# --- Define Top Materials and Color Palette ---
top_materials <- material_expanded |>
  count(primary_material, wt = area_km2, sort = TRUE) |>
  slice_head(n = top_n_materials) |>
  pull(primary_material)

material_area_order <- material_expanded |>
  mutate(primary_material = ifelse(is.na(primary_materials_list), "Unknown", primary_materials_list)) |>
  mutate(primary_material = ifelse(primary_material %in% top_materials, primary_material, "Other")) |>
  count(primary_material, wt = area_km2, sort = TRUE) |>
  arrange(n) |>
  pull(primary_material)

material_levels <- unique(c(material_area_order, "Unknown"))
material_palette <- setNames(
  if (length(material_levels) <= 8) {
    RColorBrewer::brewer.pal(length(material_levels), "Set2")
  } else {
    hcl.colors(length(material_levels), "Dynamic")
  },
  material_levels
)

# --- Prepare Country Plot Data ---
top_countries <- material_expanded |>
  count(country_name, wt = area_km2, sort = TRUE) |>
  slice_head(n = top_n_countries) |>
  pull(country_name)

data_country <- material_expanded |>
  mutate(
    country_group = ifelse(country_name %in% top_countries, country_name, "Rest of World"),
    material_group = case_when(
      is.na(primary_material) ~ "Unknown",
      primary_material %in% top_materials ~ primary_material,
      TRUE ~ "Other"
    ),
    material_group = factor(material_group, levels = material_levels)
  ) |>
  group_by(country_group, material_group) |>
  summarise(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop") |>
  group_by(country_group) |>
  mutate(total_area_country = sum(area_km2)) |>
  ungroup() |>
  mutate(
    country_group = fct_reorder(country_group, total_area_country, .desc = FALSE)
  )

# --- Prepare Biome Plot Data ---
data_biome <- material_expanded |>
  filter(biome_name != "N/A" & !is.na(biome_name)) |>
  mutate(
    material_group = case_when(
      is.na(primary_material) ~ "Unknown",
      primary_material %in% top_materials ~ primary_material,
      TRUE ~ "Other"
    ),
    material_group = factor(material_group, levels = material_levels)
  ) |>
  group_by(biome_name, material_group) |>
  summarise(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop") |>
  group_by(biome_name) |>
  mutate(total_area_biome = sum(area_km2)) |>
  ungroup() |>
  mutate(
    biome_name = fct_reorder(biome_name, total_area_biome, .desc = FALSE)
  )

# --- Prepare Bar/Treemap Plot Data ---
area_summary <- material_expanded |>
  group_by(primary_material) |>
  summarise(area_km2 = sum(area_km2, na.rm = TRUE), .groups = "drop") |>
  mutate(
    material_group = case_when(
      is.na(primary_material) ~ "Unknown",
      primary_material %in% top_materials ~ primary_material,
      TRUE ~ "Other"
    ),
    material_group = factor(material_group, levels = material_levels)
  ) |>
  group_by(material_group) |>
  summarise(total_area_km2 = sum(area_km2), .groups = "drop") |>
  arrange(desc(total_area_km2)) |>
  mutate(
    p.total_area_km2 = total_area_km2 / sum(total_area_km2),
    label = paste0(material_group, "\n", sprintf("%.1f%%", p.total_area_km2 * 100))
  )

# --- Prepare Map Plot Data ---
cat("Preparing gridded map data...\n")
sf_use_s2(TRUE) # Enable S2 for grid operations

# Load world map background
world_map <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(admin != "Antarctica") |>
  st_transform(crs = "+proj=robin") |>
  st_union() |>
  st_make_valid()  

# Load polygon features and transform to Robinson
plot_map_polygons <- cluster_features_sf |>
  filter(str_starts(id, "A")) |>
  st_transform(crs = "+proj=robin") |>
  # Join material info (simplified)
  left_join(
    select(material_expanded, id, primary_material, area_km2), 
    by = "id"
  ) |>
  mutate(
    material_group = case_when(
      is.na(primary_material) ~ "Unknown",
      primary_material %in% top_materials ~ primary_material,
      TRUE ~ "Other"
    ),
    material_group = factor(material_group, levels = material_levels)
  )

# Create a 50km grid
global_grid <- st_make_grid(plot_map_polygons, cellsize = c(50000, 50000)) |>
  st_as_sf() |>
  mutate(grid_id = 1:n())

# Join and find dominant material
grid_join <- st_join(global_grid, plot_map_polygons) |>
  filter(!is.na(material_group))

grid_summary <- grid_join |>
  st_drop_geometry() |>
  group_by(grid_id) |>
  count(material_group, wt = area_km2, sort = TRUE) |>
  slice(1) |>
  ungroup() |>
  rename(dominant_material = material_group)

grid_to_plot <- global_grid |>
  inner_join(grid_summary, by = "grid_id") 

sf_use_s2(FALSE) # Disable S2 again
cat("Plotting data is ready.\n")

# -----------------------------------------------------------------
# PART 4: CREATE PLOT OBJECTS
# -----------------------------------------------------------------
cat("Generating plot objects...\n")

# --- PLOT A: Global Gridded Map ---
map_plot <- ggplot() +
  geom_sf(data = world_map, fill = "#f0f0f0", color = "#cccccc", size = 0.1) +
  geom_sf(data = grid_to_plot, aes(fill = dominant_material), color = NA) +
  scale_fill_manual(
    values = material_palette, 
    limits = names(material_palette),
    name = "Dominant Material"
  ) +
  scale_y_continuous(limits = st_bbox(world_map)[c("ymin", "ymax")]) +
  scale_x_continuous(limits = c(-14000000, 15000000)) + #st_bbox(world_map)[c("xmin", "xmax")]
  theme_void(base_size = 5) +
  theme(
    legend.position = c(0.0, 0.1), 
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = alpha("white", 0.6), color = NA),
    legend.key.size = unit(0.2, "cm")
  )

# --- PLOT B: Bar Plot (Area by Material) ---
bar_plot <- ggplot(area_summary, 
                   aes(x = reorder(material_group, desc(total_area_km2)), 
                       y = total_area_km2, 
                       fill = material_group)) +
  geom_col() +
  geom_text(
    aes(label = sprintf("%.1f%%", p.total_area_km2 * 100)), 
    vjust = -1,
    size = 3,
    angle = 0,
    hjust = 0
  ) +
  scale_fill_manual(values = material_palette, limits = names(material_palette), name = "Materials") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  coord_cartesian(clip = "off") +
  labs(
    x = "", 
    y = "Area (thousand km²)",
    title = "",
  ) +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(5.5, 25, 5.5, 5.5, "pt")
  ) 

# --- PLOT C: Treemap Plot ---
plot_treemap <- ggplot(area_summary, aes(area = total_area_km2, fill = material_group, label = label)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = FALSE, size = 9) +
  scale_fill_manual(values = material_palette) +
  theme_minimal(base_size = 6) +
  theme(legend.position = "none")

# --- PLOT D: Country Plot ---
label_country <- data_country |>
  group_by(country_group) |>
  summarise(area_km2 = sum(area_km2), .groups = "drop") |>
  mutate(label_y = area_km2 + max(area_km2) * 0.02)

plot_country <- ggplot(data_country, aes(x = fct_rev(country_group), y = area_km2, fill = material_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = material_palette, name = "Materials") +
  geom_text(
    data = label_country,
    aes(x = fct_rev(country_group), y = label_y, label = country_group),
    inherit.aes = FALSE,
    angle = 20, hjust = 0, vjust = 0.5, size = 3
  ) +
  labs(x = NULL, y = NULL, fill = "Material") +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = c(.9, .7),
    plot.margin = margin(1, 25, 1, 2)
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 40000)) # Note: Original had hardcoded limit

# --- PLOT E: Biome Plot ---
label_biome <- data_biome |>
  group_by(biome_name) |>
  summarise(area_km2 = sum(area_km2), .groups = "drop") |>
  mutate(label_y = area_km2 + max(area_km2) * 0.02)

plot_biome <- ggplot(data_biome, aes(x = fct_rev(biome_name), y = area_km2, fill = material_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = material_palette) +
  geom_text(
    data = label_biome,
    aes(x = fct_rev(biome_name), y = label_y, label = biome_name),
    inherit.aes = FALSE,
    angle = 20, hjust = 0, vjust = 0.5, size = 3
  ) +
  labs(x = NULL, y = NULL, fill = "Material") +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(1, 120, 1, 2)
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 40000)) # Note: Original had hardcoded limit

# -----------------------------------------------------------------
# PART 5: ASSEMBLE AND SAVE PLOTS
# -----------------------------------------------------------------
cat("Assembling and saving final plots...\n")

# --- Assemble Integrated Figure ---
# Assemble the right column: (c) stacked on top of (d)
right_column <- plot_country / plot_biome

# Assemble the bottom row: (b) on the left and the right_column on the right
bottom_row <- bar_plot | right_column + 
  plot_layout(widths = unit(c(12, 1), c('cm', 'cm')))

# Assemble the final figure: (a) on top of the bottom_row
integrated_plot <- bottom_row + 
  plot_layout(heights = c(1, 2)) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") # Adds (a), (b), (c) labels

# Save the new integrated plot
png(filename = file.path(output_dir, "plot_integrated_overview.png"), width = 3000, height = 2000, res = 300)
print(integrated_plot)
dev.off()
cat("Saved 'plot_integrated_overview.png'\n")

# --- Save Individual Plots (as in original script) ---
png(filename = file.path(output_dir, "plot_country_overview.png"), width = 1800, height = 1800, res = 300)
print(plot_country + labs(title = NULL)) # Remove title for individual save
dev.off()

png(filename = file.path(output_dir, "plot_biome_overview.png"), width = 1800, height = 1800, res = 300)
print(plot_biome + labs(title = NULL))
dev.off()

png(filename = file.path(output_dir, "plot_material_overview.png"), width = 1800, height = 600, res = 300)
print(plot_treemap + labs(title = NULL))
dev.off()

png(filename = file.path(output_dir, "plot_global_top_commodity_bar.png"), width = 600, height = 1800, res = 300)
print(bar_plot)
dev.off()

png(filename = file.path(output_dir, "plot_global_top_commodity_finer_grid.png"), width = 1200, height = 600, res = 300)
print(map_plot)
dev.off()

cat("All plots saved.\n")

# -----------------------------------------------------------------
# PART 6: FINAL DATA EXPORTS & CONSOLE SUMMARIES
# -----------------------------------------------------------------
cat("Exporting supplementary data...\n")

# --- Export Supplementary Area Accounting ---
material_expanded |>
  filter(str_starts(id, "A")) |>
  mutate(primary_materials_list = primary_material, area_mine = area_km2) |>
  mutate(n_primary_materials = n_materials, .after = "primary_materials_list") |>
  select(-primary_material, -area_km2, -n_materials, -data_source) |>
  rename(id_polygon = id, primary_commodity = primary_materials_list, n_primary_commodities = n_primary_materials, commodities_list = materials_list, area_mine_km2 = area_mine) |>
  write_csv(file.path(output_dir, "s02-mine_area_accounting.csv"))

# --- Export S&P-filtered Polygons ---
mine_polygons <- st_read(str_c(output_dir, "/mine_polygons.gpkg"))
mine_points <- st_read(str_c(output_dir, "/mine_points.gpkg")) |>
  st_drop_geometry() |>
  select(id_cluster, point_source = data_source) |>
  as_tibble() |>
  group_by(id_cluster) |>
  summarise(point_source = str_c(unique(point_source), collapse = ","))

subset_mine_polygons <- left_join(mine_polygons, mine_points, by = "id_cluster") |>
  filter(point_source != "S&P" | is.na(point_source)) |>
  select(-point_source, -data_source)

st_write(subset_mine_polygons, file.path(output_dir, "subset_mine_polygons.gpkg"), delete_dsn = TRUE)
cat("Data exports complete.\n")

# --- Final Console Summaries ---
cat("\n--- Final Summaries ---\n")

cat("Total Area by Country (Top 15):\n")
data_country |> 
  group_by(country_group) |>
  reframe(area_mine_km2 = sum(area_km2)) |>
  mutate(p.area_mine = area_mine_km2 / sum(area_mine_km2) * 100) |>
  arrange(desc(p.area_mine)) |>
  print()
cat("\nTotal area (Country):", sum(data_country$area_km2), "km²\n")

cat("\nTotal Area by Biome:\n")
data_biome |> 
  group_by(biome_name) |>
  reframe(area_mine_km2 = sum(area_km2)) |>
  mutate(p.area_mine = area_mine_km2 / sum(area_mine_km2) * 100) |>
  arrange(desc(p.area_mine)) |>
  print()
cat("\nTotal area (Biome):", sum(data_biome$area_km2), "km²\n")

cat("\nSubset Polygon Area (S&P excluded):\n")
cat(sum(subset_mine_polygons$area_mine) * 1e-6, "km²\n")
group_by(subset_mine_polygons, is.na(primary_materials_list)) |>
  st_drop_geometry() |>
  summarise(area_mine = sum(area_mine)*1e-6) |>
  mutate(perc = area_mine / sum(area_mine)) |>
  print()

cat("\n--- Script 04 Complete ---\n")