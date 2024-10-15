library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(sf)
library(ggplot2)
library(scales)
library(viridis)

release_version <- "v1"
release_dir <- str_c("./output/data-release-", release_version)
dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)

mine_clusters_concordance <- read_csv("./output/mine_clusters_concordance.csv") 

mine_polygons <- st_read("./data/cluster_data.gpkg") |>
  filter(str_detect(id, "A")) |>
  select(id, data_source, area_mine) |>
  left_join(select(mine_clusters_concordance, -area_mine))  |>
  select(id, id_cluster, data_source, area_mine, primary_materials_list, materials_list, geom)

st_write(mine_polygons, dsn = str_c(release_dir, "/mine_polygons.gpkg"), layer = "mine_polygons", delete_dsn = TRUE)
st_write(mine_polygons, dsn = str_c(release_dir, "/mine_polygons.shp"), layer = "mine_polygons", delete_dsn = TRUE)

