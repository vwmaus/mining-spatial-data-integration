library(sf)
library(dplyr)
library(stringr)

mine_polygons <- st_read("./output/20241026-all_materials/mine_polygons.gpkg")

mines <- st_drop_geometry(mine_polygons) |>
    as_tibble()

# How many polygons do you have in your latest data set in total?
cat(" Number of polygons: ", nrow(mines))

# What is the estimated land area in total?
cat(" Total area [m2]: ", sum(mines$area_mine))

# How many polygons are related to copper mining as main or by-product?
cat(" Number of polygons with copper as primary material: ", nrow(filter(mines, str_detect(primary_materials_list, "Copper"))))
cat(" Number of polygons with copper as by-product: ", nrow(filter(mines, str_detect(materials_list, "Copper") & !str_detect(primary_materials_list, "Copper"))))

# And what land area is affected by mining that involves copper as main or by-product?
cat(" Area with copper as primary material [m2]: ", sum(filter(mines, str_detect(primary_materials_list, "Copper"))$area_mine))
cat(" Are with copper as by-product [m2]: ", sum(filter(mines, str_detect(materials_list, "Copper") & !str_detect(primary_materials_list, "Copper"))$area_mine))

