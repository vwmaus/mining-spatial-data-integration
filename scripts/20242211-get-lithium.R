library(sf)
library(dplyr)
library(readr)
library(stringr)

release_version_name <- "lithium-mines"
release_version_date <- format(Sys.Date(), '%Y%m%d')
path_output <- str_c("./output/", release_version_date, "-", release_version_name)
dir.create(path_output, recursive = TRUE, showWarnings = FALSE)

mine_points <- st_read("./output/20241121-all_materials/mine_points.gpkg")

mine_polygons <- st_read("./output/20241121-all_materials/mine_polygons.gpkg")

st_drop_geometry(mine_points) |>
    as_tibble() |>
    select(id_cluster, materials_data_source = data_source) |>
    distinct() |>
    group_by(id_cluster) |>
    reframe(materials_data_source = str_c(materials_data_source, collapse = ";")) |>
    right_join(mine_polygons) |>
    filter(str_detect(materials_list, "Lithium")) |>
    rename(polygon_data_source = data_source) |>
    st_write(str_c(path_output, "/lithium_polygons.gpkg"), layer = "mine_polygons")
