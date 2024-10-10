library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(sf)

release_version <- "v1"
release_dir <- str_c("./output/data-release-", release_version)
dir.create(release_dir, recursive = TRUE, showWarnings = FALSE)

#### TODO:
# 1. Define which data to release:
#   - id, id_cluster, data_source, primary_commodity_list, commodities_list, area_mine, geometry
#    other variables, should not be included in this repo, but be added on demand, depending on the application
# 2. Implement the above election
#   - Link the polygons to clusters



mutate(final_cluster_concordance,
       has_coal = str_detect(primary_materials_list, "Coal"),
       has_gold = str_detect(primary_materials_list, "Gold"),
       mat_group = ifelse(has_coal & has_gold, "Coal+Gold", ifelse(has_gold, "Gold", ifelse(has_coal, "Coal", "Other")))) |>
  group_by(mat_group) |>
  reframe(area_mine = sum(area_mine, na.rm = TRUE)) |>
  mutate(perc = area_mine / sum(area_mine))




st_write(mine_polygons, dsn = str_c(release_dir, "/mine_polygons.gpkg"), layer = "mine_polygons", delete_dsn = TRUE)
st_write(mine_polygons, dsn = str_c(release_dir, "/mine_polygons.shp"), layer = "mine_polygons", delete_dsn = TRUE)

mine_polygons <- st_read(dsn = str_c(release_dir, "/mine_polygons.shp"))



















