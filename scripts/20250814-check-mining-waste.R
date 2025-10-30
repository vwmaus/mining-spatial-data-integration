library(sf)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)

mine_cluster <- st_read("./output/20250606-all_materials/cluster_features.gpkg")

maus2022 <- filter(mine_cluster, str_detect(data_source, "Maus")) |>
    st_join(st_read("./tmp/maus/global_mining_polygons_v2.gpkg", fid_column_name = "FID"), join = st_intersects, left = TRUE) |>
    filter(!is.na(FID))

mine_cluster_tbl <- read_csv("./output/20250606-all_materials/clusters_datasource_concordance.csv") |>
    select(id, id_cluster, id_data_source, data_source) |>
    filter(str_detect(data_source, "Jasansky"))

maus2022_contbl <- filter(maus2022, id_cluster %in% unique(pull(mine_cluster_tbl, id_cluster))) |>
    st_drop_geometry() |>
    as_tibble() |>
    select(id_cluster, AREA, FID) |>
    distinct() 

jasansky2023_contbl <- select(mine_cluster_tbl, id_cluster, id_jasansky = id_data_source)

# cannot have duplicated polygons or polygons
any(duplicated(maus2022_contbl$FID))
any(duplicated(jasansky2023_contbl$id_jasansky))

waste_cluster <- read_csv("./tmp/jasansky/data/waste.csv") |>
    filter(facility_id %in% unique(jasansky2023_contbl$id_jasansky)) |>
    select(id_jasansky = facility_id, year, waste_type, value_tonnes, total_material_tonnes) |>
    left_join(jasansky2023_contbl) |>
    filter(!is.na(value_tonnes) | is.na(total_material_tonnes)) |>
    group_by(id_cluster, year) |>
    reframe(waste_material_tonnes = sum(value_tonnes, na.rm = TRUE), excavated_material_tonnes = sum(total_material_tonnes, na.rm = TRUE)) |>
    mutate(waste_material_tonnes = ifelse(waste_material_tonnes==0,NA,waste_material_tonnes), excavated_material_tonnes=ifelse(excavated_material_tonnes==0,NA,excavated_material_tonnes)) |>
    left_join(maus2022_contbl, relationship = "many-to-many")

waste_cluster_group <- waste_cluster |>
    group_by(id_cluster) |>
    reframe(
        waste_material_tonnes = sum(waste_material_tonnes) / length(unique(FID)),
        excavated_material_tonnes = sum(excavated_material_tonnes) / length(unique(FID)),
        AREA = sum(AREA) / length(unique(year)))

waste_cluster_group |>
    pivot_longer(cols = c(waste_material_tonnes, excavated_material_tonnes)) |>
    ggplot(aes(y = log(value), x = log(AREA))) +
    facet_wrap(~name, scales = "free") +
    geom_smooth(method='lm', formula= y~x) +
    geom_point()

lm_waste <- lm(log(waste_material_tonnes) ~ log(AREA), waste_cluster_group)
summary(lm_waste)

lm_excavated <- lm(log(excavated_material_tonnes) ~ log(AREA), waste_cluster_group)
summary(lm_excavated)

# The area alone does not explain the tonnes - R-squared is too low
# Other factors would need to be taken into account
