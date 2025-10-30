library(sf)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

mine_polygons <- st_read("./output/20250606-all_materials/mine_polygons.gpkg") |>
    select(materials_list, area_mine) |>
    st_drop_geometry() |>
    as_tibble()

# How many polygons do you have in your latest data set in total?
cat(" Number of polygons: ", nrow(mine_polygons))

# What is the estimated land area in total?
cat(" Total area [m2]: ", sum(mine_polygons$area_mine))

# What is the estimated land area in total?
cat(" Number of materials: ", length(unique(separate_rows(mine_polygons, materials_list, sep=",")$materials_list)))

