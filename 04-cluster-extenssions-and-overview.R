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

data_version <- "20251030-all_materials"

# Country distribution 
sf_use_s2(FALSE)

cluster_features <- st_read(str_c("./output/", data_version, "/cluster_features.gpkg"))

if(!file.exists(str_c("./output/", data_version, "/country_tbl.csv"))){
    
    st_centroid(cluster_features) |>
        st_join(select(ne_countries(scale = "medium", returnclass = "sf"), country_name = admin, country_isoa3 = adm0_a3), join = st_nearest_feature) |>
        st_drop_geometry() |>
        as_tibble() |>
        select(id, country_name, country_isoa3) |>
        write_csv(str_c("./output/", data_version, "/country_tbl.csv"))

}

if(!file.exists(str_c("./output/", data_version, "/ecoregion_tbl.csv"))){

    if(!file.exists("./tmp/ecoregions/ecoregions.gpkg")){
        download.file("https://storage.googleapis.com/teow2016/Ecoregions2017.zip", destfile = "./tmp/Ecoregions2017.zip")
        unzip("./tmp/Ecoregions2017.zip", exdir = "./tmp/ecoregions/")
        st_read("./tmp/ecoregions/Ecoregions2017.shp") |>
          select(ecoregion_name = ECO_NAME, biome_name = BIOME_NAME) |>
          st_simplify() |>
          st_write("./tmp/ecoregions/ecoregions.gpkg")
    }

    st_centroid(cluster_features) |>
        st_join(st_read("./tmp/ecoregions/ecoregions.gpkg"), join = st_nearest_feature) |>
        st_drop_geometry() |>
        as_tibble() |>
        select(id, ecoregion_name = ecoregions_name, biome_name) |>
        write_csv(str_c("./output/", data_version, "/ecoregion_tbl.csv"))
        
}

cluster_features <- cluster_features |>
    st_drop_geometry() |>
    as_tibble() |>
    left_join(read_csv(str_c("./output/", data_version, "/country_tbl.csv"))) |>
    left_join(read_csv(str_c("./output/", data_version, "/ecoregion_tbl.csv")))

#### Results summary assigned 
summary_tbl <- cluster_features |>
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
    writeLines(con = str_c("./output/", data_version, "/tbl-cluster-summary.tex"))



#### Results summary data source 
cluster_features |>
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
  writeLines(con = str_c("./output/", data_version, "/tbl-polygon-source-summary.tex"))


##### Summary figures 

# -------------------------------
# Parameters
# -------------------------------
top_n_countries <- 15
top_n_materials <- 7

# -------------------------------
# STEP 1: Expand material list and adjust area
# -------------------------------
material_expanded <- cluster_features |>
  separate_rows(primary_materials_list, sep = ",\\s*") |>
  group_by(id) |>
  mutate(n_materials = n()) |>
  ungroup() |>
  mutate(
    area_km2 = (area_mine / n_materials) * 1e-6,
    primary_material = ifelse(is.na(primary_materials_list), "Unknown", primary_materials_list)
  )

# -------------------------------
# STEP 2: Compute top materials and countries
# -------------------------------
top_materials <- material_expanded |>
  count(primary_material, wt = area_km2, sort = TRUE) |>
  slice_head(n = top_n_materials) |>
  pull(primary_material)

# Order material levels by total area descending
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

# -------------------------------
# STEP 3: Summarize and clean for plotting (Country)
# -------------------------------
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

# -------------------------------
# PLOT 1: Country vs Area by Material (Top Countries)
# -------------------------------
label_country <- data_country |>
  group_by(country_group) |>
  summarise(area_km2 = sum(area_km2), .groups = "drop") |>
  mutate(label_y = area_km2 + max(area_km2) * 0.02)

plot_country <- ggplot(data_country, aes(x = fct_rev(country_group), y = area_km2, fill = material_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = material_palette) +
  geom_text(
    data = label_country,
    aes(x = fct_rev(country_group), y = label_y, label = country_group),
    inherit.aes = FALSE,
    angle = 20, hjust = 0, vjust = 0.5, size = 3
  ) +
  labs(x = NULL, y = "Area (km²)", fill = "Material", title = "(a)") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    plot.margin = margin(1, 25, 1, 2)  # Increased top margin
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 40000))

# -------------------------------
# STEP 4: Summarize and clean for plotting (Biome)
# -------------------------------
data_biome <- material_expanded |>
  filter(biome_name != "N/A") |>
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

# -------------------------------
# PLOT 2: Biome vs Area by Material (Absolute Values)
# -------------------------------
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
  labs(x = NULL, y = NULL, fill = "Material", title = "(b)") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(1, 120, 1, 2)  # Increased top margin
  ) +
  coord_cartesian(clip = "off", ylim = c(0, 40000))

# -------------------------------
# PLOT 3: Treemap of Area by Material (Top 7)
# -------------------------------
material_treemap_data <- material_expanded |>
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
  summarise(area_km2 = sum(area_km2), .groups = "drop") |>
  mutate(
    pct = 100 * area_km2 / sum(area_km2),
    label = paste0(material_group, "\n", sprintf("%.1f%%", pct))
  )

plot_treemap <- ggplot(material_treemap_data, aes(area = area_km2, fill = material_group, label = label)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = FALSE, size = 6) +
  scale_fill_manual(values = material_palette) +
  theme_minimal(base_size = 6) +
  labs(title = "(c)") +
  theme(legend.position = "none")

# -------------------------------
# Show and Save Plots
# -------------------------------

output_dir <- str_c("./output/", data_version)

png(filename = file.path(output_dir, "plot_country_overview.png"), width = 1800, height = 1800, res = 300)
print(plot_country)
dev.off()

png(filename = file.path(output_dir, "plot_biome_overview.png"), width = 1800, height = 1800, res = 300)
print(plot_biome)
dev.off()

png(filename = file.path(output_dir, "plot_material_overview.png"), width = 1800, height = 600, res = 300)
print(plot_treemap)
dev.off()

material_expanded |>
  filter(str_starts(id, "A")) |>
  mutate(primary_materials_list = primary_material, area_mine = area_km2) |>
  mutate(n_primary_materials = n_materials, .after = "primary_materials_list") |>
  select(-primary_material, -area_km2, -n_materials, -data_source) |>
  rename(id_polygon = id, primary_commodity = primary_materials_list, n_primary_commodities = n_primary_materials, commodities_list = materials_list, area_mine_km2 = area_mine) |>
  write_csv(file.path(output_dir, "s02-mine_area_accounting.csv"))

data_country |> 
  group_by(country_group) |>
  reframe(area_mine_km2 = sum(area_km2)) |>
  mutate(p.area_mine = area_mine_km2 / sum(area_mine_km2) * 100) |>
  arrange(desc(p.area_mine))

cat("\nTotal area:", sum(data_country$area_km2), "\n")

data_biome |> 
  group_by(biome_name) |>
  reframe(area_mine_km2 = sum(area_km2)) |>
  mutate(p.area_mine = area_mine_km2 / sum(area_mine_km2) * 100) |>
  arrange(desc(p.area_mine))

cat("\nTotal area:", sum(data_biome$area_km2), "\n")