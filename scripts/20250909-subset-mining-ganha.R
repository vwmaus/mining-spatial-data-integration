library(sf)
library(dplyr)
library(stringr)
library(readr)

mine_polygons <- st_read("./output/20250606-all_materials/mine_polygons.gpkg") |>
    left_join(read_csv("./output/20250606-all_materials/country_tbl.csv")) |>
    filter(country_name == "Ghana")

# How many polygons do you have in your latest data set in total?
cat(" Number of polygons: ", nrow(mine_polygons))

# What is the estimated land area in total?
cat(" Total area [m2]: ", sum(mine_polygons$area_mine))

st_write(select(mine_polygons, -primary_materials_list, -materials_list), "./scripts/data/mine_polygons_ghana.gpkg", delete_dsn = TRUE)
