# This script finds the optimal threshold for clustering the spatial data.
# It minimizes the area with unknown primary material and the area with multiple primary materials.
# It also performs data checks an comparisons between results of local and global optimal
# The local optimal usually provides better results and are set as default to produce the data release
# 
################################################################################
########################### REQUIRED PARAMETERS ################################
# the maximum distance for the global optimization 
max_global_threshold <- 20
# the maximum distance for the local optimization 
max_local_threshold <- 10
# concordance table to harmonise materials. Set to NULL to keep as they are
path_harmonisation_table <- "./data/harmonisation_all_materials.csv"
# set version of the data output
release_version_name <- "all_materials"
release_version_date <- format(Sys.Date(), '%Y%m%d')
################################################################################

# Require libraries
library(sf)
library(stringr)
library(dplyr)
library(tibble)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringi)
library(purrr)
library(car)

# Plot colours
red_d <- "#cc3e5b"
green_d <- "#306056"
yellow_d <- "#f6ae2d"

path_output <- str_c("./output/", release_version_date, "-", release_version_name)
dir.create(path_output, recursive = TRUE, showWarnings = FALSE)

id_to_remove <- tibble(id=NULL)
if(file.exists("./data/incorrect_polygons_list.csv")) id_to_remove <- read_csv("./data/incorrect_polygons_list.csv")

hcluster_concordance <- read_csv("./tmp/hcluster_concordance.csv") |>
  left_join(st_read(dsn = "./tmp/cluster_data.gpkg", query = "SELECT id, area_mine FROM cluster_data", quiet = TRUE)) |>
  filter(!id %in% id_to_remove$id) # Polygon with error 

if(!is.null(path_harmonisation_table)){
  source("./R/harmonize_materials.R")
  mapping_table <- read_csv(path_harmonisation_table)
  hcluster_concordance <- hcluster_concordance |>
    mutate(across(all_of(starts_with("primary_")), ~ harmonize_materials(.x, mapping_table, col_from = "material", col_to = "material_harmonized")),
           across(all_of(starts_with("list_")), ~ harmonize_materials(.x, mapping_table, col_from = "material", col_to = "material_harmonized")))
}

# Filter global optimal threshold - Pareto Optimal Point
materials_area_global <- hcluster_concordance |>
  mutate(id_max_threshold := !!sym(str_c("id_hc_", max_global_threshold))) |>
  transmute(id, area_mine,
            across(all_of(starts_with("primary_")), ~ str_count(.x, ",") + 1, .names = "count{.col}"),
            across(all_of(starts_with("list_")), ~ str_count(.x, ",") + 1, .names = "count{.col}"),
            across(all_of(starts_with("primary_")), ~ is.na(.x))) |>
  select(-starts_with("list_")) |>
  pivot_longer(all_of(matches("primary|list")), names_to = c("var", "clust", "clust_dist"), values_to = "value", names_sep = "_") |>
  select(-clust) |>
  mutate(clust_dist = as.numeric(clust_dist)) |>
  filter(clust_dist <= max_global_threshold) |>
  pivot_wider(names_from = var, values_from = value) |>
  rename(unknown = primary)

pareto_optimal_point_global <- materials_area_global |>
  group_by(clust_dist) |>
  reframe(area_na = sum(area_mine, na.rm = TRUE),
          area_na = area_na / area_na,
          area_known = area_na * sum(area_mine * (!unknown), na.rm = TRUE),
          area_unknown = area_na * sum(area_mine * (unknown), na.rm = TRUE),
          area_primary = area_na * sum(area_mine * countprimary, na.rm = TRUE),
          area_list = area_na * sum(area_mine * countlist, na.rm = TRUE),
          area_single_primary = area_na * sum(area_mine * (countprimary==1), na.rm = TRUE),
          area_mixed_primary = area_na * sum(area_mine * (countprimary>1), na.rm = TRUE),
          area_multi_count = area_na * sum(area_mine * (countprimary - 1), na.rm = TRUE),
          area_mine = area_na * sum(area_mine, na.rm = TRUE),
          perc_area_known = area_known / area_mine,
          perc_area_unknown = area_unknown / area_mine,
          perc_area_primary = area_primary / area_mine,
          perc_area_list = area_list / area_mine,
          perc_area_mixed_primary = area_mixed_primary / area_mine,
          perc_area_multi_count = area_multi_count / area_mine
          ) |>
          mutate(distances = sqrt(perc_area_multi_count/max(c(perc_area_multi_count, 1e-12)))^2 + (perc_area_unknown/max(c(perc_area_unknown, 1e-12)))^2,
                 pareto_index = clust_dist[which.min(distances)])

# select(pareto_optimal_point_global, clust_dist, perc_area_known, perc_area_unknown, perc_area_multi_count, distances, pareto_index)

# max threshold defined be visual check on the figure 
materials_area_local <- hcluster_concordance |>
  mutate(id_max_threshold := !!sym(str_c("id_hc_", max_local_threshold))) |>
  transmute(id, id_max_threshold, area_mine,
            across(all_of(starts_with("primary_")), ~ str_count(.x, ",") + 1, .names = "count{.col}"),
            across(all_of(starts_with("list_")), ~ str_count(.x, ",") + 1, .names = "count{.col}"),
            across(all_of(starts_with("primary_")), ~ is.na(.x))) |>
  select(-starts_with("list_")) |>
  pivot_longer(all_of(matches("primary|list")), names_to = c("var", "clust", "clust_dist"), values_to = "value", names_sep = "_") |>
  select(-clust) |>
  mutate(clust_dist = as.numeric(clust_dist)) |>
  filter(clust_dist <= max_local_threshold) |>
  pivot_wider(names_from = var, values_from = value) |>
  rename(unknown = primary)

pareto_optimal_point_local <- materials_area_local |>
  group_by(clust_dist, id_max_threshold) |>
  reframe(area_na = sum(area_mine, na.rm = TRUE),
          area_na = area_na / area_na,
          area_known = area_na * sum(area_mine * (!unknown), na.rm = TRUE),
          area_unknown = area_na * sum(area_mine * (unknown), na.rm = TRUE),
          area_primary = area_na * sum(area_mine * countprimary, na.rm = TRUE),
          area_list = area_na * sum(area_mine * countlist, na.rm = TRUE),
          area_single_primary = area_na * sum(area_mine * (countprimary==1), na.rm = TRUE),
          area_mixed_primary = area_na * sum(area_mine * (countprimary>1), na.rm = TRUE),
          area_multi_count = area_na * sum(area_mine * (countprimary - 1), na.rm = TRUE),
          area_mine = area_na * sum(area_mine, na.rm = TRUE),
          perc_area_known = area_known / area_mine,
          perc_area_unknown = area_unknown / area_mine,
          perc_area_primary = area_primary / area_mine,
          perc_area_list = area_list / area_mine,
          perc_area_mixed_primary = area_mixed_primary / area_mine,
          perc_area_multi_count = area_multi_count / area_mine
          ) |>
          group_by(id_max_threshold) |>
          mutate(distances = sqrt(perc_area_multi_count/max(c(perc_area_multi_count, 1e-12)))^2 + (perc_area_unknown/max(c(perc_area_unknown, 1e-12)))^2,
                 pareto_index = ifelse(all(is.na(distances)), min(clust_dist), clust_dist[which.min(distances)])) |>
          ungroup()

# select(pareto_optimal_point_local, perc_area_known, perc_area_unknown, perc_area_multi_count, distances, pareto_index)

pareto_optimal_point_local_summary <- select(pareto_optimal_point_local, clust_dist, pareto_index, area_mine, area_known, area_unknown, area_multi_count) |>
  filter(clust_dist == pareto_index) |>
  summarise(across(all_of(starts_with("area")), ~sum(.x, na.rm = TRUE))) |>
  mutate(
      perc_area_known = area_known / area_mine,
      perc_area_unknown = area_unknown / area_mine,
      perc_area_multi_count = area_multi_count / area_mine,
      clust_dist = "",
      approach = str_c("Local (max threshold ", max_local_threshold, " km)"))

pareto_optimal_point_global_summary <- select(pareto_optimal_point_global, clust_dist, pareto_index, area_mine, area_known, area_unknown, area_multi_count) |>
  filter(clust_dist == pareto_index) |>
  summarise(across(all_of(starts_with("area")), ~sum(.x)), clust_dist = as.character(clust_dist)) |>
  mutate(
      perc_area_known = area_known / area_mine,
      perc_area_unknown = area_unknown / area_mine,
      perc_area_multi_count = area_multi_count / area_mine,
      approach = str_c("Global (max threshold ", max_global_threshold, " km)"))

bind_rows(
  pareto_optimal_point_local_summary,
  pareto_optimal_point_global_summary
)

gp <- select(pareto_optimal_point_local, id_max_threshold, clust_dist, pareto_index, area_mine, area_known, area_unknown, area_multi_count) |>
  filter(clust_dist == pareto_index) |>
  ggplot(aes(clust_dist)) +
    geom_histogram(binwidth = 1, fill = green_d) +
    theme_minimal() +
    labs(
      y = "Count of subclusters",
      x = "Cluster Threshold Distance (km)"
    ) 

ggsave(filename = str_c(path_output, "/fig-optimal-threshold-distribution-primary-materials.png"), plot = gp, bg = "#ffffff",
       width = 345, height = 140, units = "mm", scale = 1)

optimal_point <- bind_rows(pareto_optimal_point_local_summary, pareto_optimal_point_global_summary) |>
  reframe(x1 = c(0, 0, perc_area_multi_count),
         x2 = c(perc_area_multi_count, perc_area_multi_count),
         y1 = c(perc_area_unknown, perc_area_unknown),
         y2 = c(perc_area_unknown, 0, 0),
         clust_dist = "",
         approach = rep(approach, 2)
         )

gp <- pareto_optimal_point_global |>
  arrange(clust_dist) |> 
  mutate(clust_dist = as.character(clust_dist)) |>
  ggplot(aes(y = perc_area_unknown, x = perc_area_multi_count, label = str_c(clust_dist, " km"))) +
  geom_line(linewidth = 1) +  # Connect points in the order of clust_dist
  geom_point(size = 2) +  # Add points for emphasis
  geom_point(data = pareto_optimal_point_local_summary, colour = red_d, size = 2) +
  geom_text(hjust=-0.1, vjust=-0.5) +
  geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2, color = approach), data = optimal_point, linewidth = 0.5, linetype = "dashed") +
  geom_text(aes(x = x2+c(-0.02,-0.02), y = c(0,0), label = str_c(round(100*x2,0), "%")), 
            data = optimal_point[1:2,], vjust = 1.5, hjust = -0.1, color = "black") +
  geom_text(aes(x = 0, y = y2, label = str_c(round(y2*100, 0), "%")), 
            data = optimal_point[1:2,], vjust = 0.5, hjust = 1.2, color = "black") +
  theme_minimal() +
  scale_color_manual(name = "Threshold optimization", values = c(green_d, red_d)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    y = "Area unassigned",
    x = "Area assigned to multiple primary commodities"
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8,0.8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

ggsave(filename = str_c(path_output, "/fig-threshold-optimization-primary-materials.png"), plot = gp, bg = "#ffffff",
       width = 345, height = 140, units = "mm", scale = 1)

local_thr <- pareto_optimal_point_local |>
  filter(clust_dist == pareto_index) |>
  select(perc_area_known, perc_area_unknown, perc_area_multi_count, area_known, area_unknown, area_multi_count)

global_thr <- hcluster_concordance |>
  mutate(id_max_threshold := !!sym(str_c("id_hc_", max_global_threshold))) |>
  select(id, id_max_threshold, area_mine, ends_with(str_c("hc_", pareto_optimal_point_global_summary$clust_dist))) |>
  transmute(id, id_max_threshold, area_mine, id_cluster = !!sym(str_c("id_hc_", pareto_optimal_point_global_summary$clust_dist)),
            across(all_of(starts_with("primary_")), ~ str_count(.x, ",") + 1, .names = "count{.col}"),
            across(all_of(starts_with("primary_")), ~ is.na(.x))) |>
  pivot_longer(all_of(matches("primary|list")), names_to = c("var", "clust", "clust_dist"), values_to = "value", names_sep = "_") |>
  select(-clust) |>
  mutate(clust_dist = as.numeric(clust_dist)) |>
  pivot_wider(names_from = var, values_from = value) |>
  rename(unknown = primary) |>
  group_by(id_cluster) |>
  reframe(area_na = sum(area_mine, na.rm = TRUE),
          area_na = area_na / area_na,
          area_known = area_na * sum(area_mine * (!unknown), na.rm = TRUE),
          area_unknown = area_na * sum(area_mine * (unknown), na.rm = TRUE),
          area_primary = area_na * sum(area_mine * countprimary, na.rm = TRUE),
          area_single_primary = area_na * sum(area_mine * (countprimary==1), na.rm = TRUE),
          area_mixed_primary = area_na * sum(area_mine * (countprimary>1), na.rm = TRUE),
          area_multi_count = area_na * sum(area_mine * (countprimary - 1), na.rm = TRUE),
          area_mine = area_na * sum(area_mine, na.rm = TRUE),
          perc_area_known = area_known / area_mine,
          perc_area_unknown = area_unknown / area_mine,
          perc_area_primary = area_primary / area_mine,
          perc_area_mixed_primary = area_mixed_primary / area_mine,
          perc_area_multi_count = area_multi_count / area_mine
          ) |>
  select(perc_area_known, perc_area_unknown, perc_area_multi_count, area_known, area_unknown, area_multi_count)

#### Comparing distribution of multiple counts using difference approaches to define the threshold
compare_area_multi_count <- bind_rows(
  global_thr |>
    mutate(approach = "Global"),
  local_thr |>
    mutate(approach = "Local")) |>
  mutate(approach = factor(approach)) |>
  filter(area_multi_count > 0) |>
  select(approach, area = area_multi_count)

compare_area_unknown <- bind_rows(
  global_thr |>
    mutate(approach = "Global"),
  local_thr |>
    mutate(approach = "Local")) |>
  mutate(approach = factor(approach)) |>
  filter(area_unknown > 0) |>
  select(approach, area = area_unknown)


### Area of multiple primary materials
gp <- ggplot(compare_area_multi_count, aes(x = area, fill = approach)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(x = "Area assigned to multiple primary materials (Log10 transformed)",
       y = "Count of clusters") +
  scale_fill_viridis_d(name = "Threshold optimization", option = "D", begin = 0, end = .7) +
  scale_x_log10() +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.8, .8),
  )

ggsave(filename = str_c(path_output, "/fig-distribution-area-multiple-primary-materials.png"), plot = gp, bg = "#ffffff",
       width = 140, height = 140, units = "mm", scale = 1)

### Area with unknown materials

gp <- ggplot(compare_area_unknown, aes(x = area, fill = approach)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(x = "Area not assigned to any material (Log10 transformed)",
       y = "Count of clusters") +
  scale_fill_viridis_d(name = "Threshold optimization", option = "D", begin = 0, end = .7) +
  scale_x_log10() +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.8, .8),
  )

ggsave(filename = str_c(path_output, "/fig-distribution-area-unknown-materials.png"), plot = gp, bg = "#ffffff",
       width = 140, height = 140, units = "mm", scale = 1)

################ Assemble final cluster concordance table using local optimal 
selected_threshold <- pareto_optimal_point_local |>
  filter(clust_dist == pareto_index) |>
  select(id_max_threshold, pareto_index)

local_ids <- hcluster_concordance |>
  mutate(id_max_threshold := !!sym(str_c("id_hc_", max_local_threshold))) |>
  select(starts_with('id')) |>
  pivot_longer(all_of(matches("id_hc_")), names_to = c("var", "clust", "clust_dist"), values_to = "value", names_sep = "_") |>
  select(-c(var, clust)) |>
  left_join(selected_threshold) |>
  filter(clust_dist == pareto_index) |>
  select(id, id_cluster = value, pareto_index)

final_clusters <- select(materials_area_local, id, id_max_threshold, clust_dist) |>
  left_join(local_ids) |>
  filter(clust_dist == pareto_index) |>
  group_by(id_max_threshold, id_cluster, pareto_index) |>
  mutate(id_cluster = str_c("H", str_pad(cur_group_id(), width = 7, pad = 0))) |>
  ungroup() |>
  select(id, id_cluster, dist_threshold = clust_dist)

final_cluster_concordance <- hcluster_concordance |>
  select(-starts_with("id_hc"), -id_group) |>
  pivot_longer(all_of(matches("primary|list")), names_to = c("var", "clust", "clust_dist"), values_to = "value", names_sep = "_") |>
  select(-clust) |>
  mutate(clust_dist = as.numeric(clust_dist)) |>
  pivot_wider(names_from = "var", values_from = "value") |>
  rename(primary_materials_list = primary, materials_list = list) |>
  left_join(final_clusters) |>
  filter(clust_dist == dist_threshold) |>
  select(id, id_cluster, dist_threshold, primary_materials_list, materials_list, area_mine)

gp <- final_cluster_concordance |>
  mutate(n_primary = str_count(primary_materials_list, ","),
         n_materials = str_count(materials_list, ",") + 1) |>
  pivot_longer(c(n_primary, n_materials), names_to = "list", values_to = "n_materials") |>
  mutate(list = factor(list, c("n_materials", "n_primary"), c("All", "Primary"))) |>
  drop_na(n_materials) |>
  filter(n_materials > 0) |>
  ggplot(aes(x = n_materials, fill = list)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +
  labs(x = "Number of assigned commodities",
       y = "Count of clusters") +
  scale_fill_viridis_d(name = "List of materials", option = "D", begin = 0, end = .7, direction = -1) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.8, .8),
  )

ggsave(filename = str_c(path_output, "/fig-distribution-number-assigned-materials.png"), plot = gp, bg = "#ffffff",
       width = 140, height = 140, units = "mm", scale = 1)

cluster_features <- st_read("./tmp/cluster_data.gpkg") |>
  filter(!id %in% id_to_remove$id) |>
  select(id, data_source, id_data_source, data_source, area_mine) |>
  left_join(select(final_cluster_concordance, -area_mine))  |>
  select(id, id_cluster, id_data_source, data_source, area_mine, primary_materials_list, materials_list, geom)

release_data <- select(cluster_features, -id_data_source)

st_write(release_data, dsn = str_c(path_output, "/cluster_features.gpkg"), layer = "mine_polygons", delete_dsn = TRUE)
st_write(filter(release_data, str_detect(id, "A")), dsn = str_c(path_output, "/mine_polygons.gpkg"), layer = "mine_polygons", delete_dsn = TRUE)
st_write(filter(release_data, str_detect(id, "P")), dsn = str_c(path_output, "/mine_points.gpkg"), layer = "mine_points", delete_dsn = TRUE)
write_csv(final_cluster_concordance, str_c(path_output, "/mine_clusters.csv"))

cluster_features |>
  select(id, id_cluster, id_data_source, data_source, primary_materials_list, materials_list) |>
  write_csv(str_c(path_output, "/clusters_datasource_concordance.csv"))

cluster_features |>
  st_drop_geometry() |>
  as_tibble() |>
  filter(str_detect(id, "P")) |>
  select(id, id_cluster, id_data_source, data_source) |>
  write_csv(str_c(path_output, "/cluster_points_concordance.csv"))
