library(sf)
library(s2)
library(dplyr)
library(stringr)
library(readxl)
library(rnaturalearth)
library(rnaturalearthdata)

dir.create("./data", showWarnings = F)

# Import fast version of S2 union split agg
source("s2_union_split_agg.R")

###### Download datasets

# TODO: create data repo with this data and add as supporting material for Maus & Werner Nat. 2024

if(!file.exists("./data/maus_osm.gpkg")) {
    download.file(url = "https://zenodo.org/records/7307210/files/global_mining_polygons.gpkg?download=1", destfile = "./data/maus_osm.gpkg")
}

if(!dir.exists("./data/jasansky")){
    download.file("https://zenodo.org/records/7369478/files/open_database_mine_production.zip?download=1", destfile = "./data/jasansky.zip")
    unzip("./data/jasansky.zip", exdir = "./data/jasansky")
}

if(!file.exists("./data/gem.xlsx")){
    download.file("https://globalenergymonitor.org/wp-content/uploads/2024/04/Global-Coal-Mine-Tracker-April-2024.xlsx", destfile = "./data/gem.xlsx")
}

if(!file.exists("./data/ecoregions/ecoregions.gpkg")){
    download.file("https://storage.googleapis.com/teow2016/Ecoregions2017.zip", destfile = "./data/ecoregions.zip")
    unzip("./data/ecoregions.zip", exdir = "./data/ecoregions")
    sf_use_s2(FALSE)
    st_read("./data/ecoregions/Ecoregions2017.shp") |>
        select(ecoregions_name = ECO_NAME, biome_name = BIOME_NAME) |>
        st_cast(ecoregions, to = "POLYGON") |>
        st_simplify() |>
        st_write(dsn = "./data/ecoregions/ecoregions.gpkg")
    sf_use_s2(TRUE)
}

# Integrate mining land use datasets and and remove overlaps
if(!file.exists("./data/maus_tang_osm.gpkg")){

    # Read mining polygons from dataset combining Maus et al. 2022 and OpenStreetMap
    maus_osm <- st_read("./data/maus_osm.gpkg") |>
        select(geom)

    # Read mining polygons from dataset combining Maus et al. 2022 and Tang & Werner 2023
    maus_tang <- st_read("./data/maus_tang.gpkg") |>
        select(geom)

    mining_land_use <- bind_rows(maus_osm, maus_tang) |>
        filter(s2_is_valid(geom)) |>
        st_as_s2() |>
        s2_union_split_agg(options = s2_options(model = "closed")) |>
        st_as_sf() |>
        select(geom = "geometry") |>
        filter(st_is(geom, c("POLYGON", "MULTIPOLYGON"))) |>
        st_cast("POLYGON") |>
        st_make_valid() |>
        filter(st_is_valid(geom))

    mining_land_use <- mining_land_use |>
        mutate(
            id = str_c("A", str_pad(row_number(), width = 7, pad = "0")),
            id_data_source = str_c("id", row_number()), # TODO: replace with original data ID
            area_mine = st_area(geom),
            data_type = "land-use",
            data_source = "Maus et al. 2022; Tang & Werner 2023; OpenStreetMap",
            ) |>
            select(id, id_data_source, data_type, data_source, geom)

    st_write(mining_land_use, "./data/maus_tang_osm.gpkg", delete_layer = TRUE)

} else {
   mining_land_use <- st_read("./data/maus_tang_osm.gpkg")
}

# Integrate mining properties
if (!file.exists("./data/mining_properties.gpkg")) {

    # Read mining properties from Jasansky et al. 2022
    jasansky <- st_read("./data/jasansky/data/facilities.gpkg") |>
        select(id_data_source = facility_id, primary_commodity, commodities_list = commodities_products) |>
        mutate(commodities_list = str_replace_all(commodities_list, ", ", ","), data_source = "Jasansky et al. 2022") |>
        filter(primary_commodity != "Processing")

    # Read mining properties from S&P
    sp <- st_read("./data/snl2020.gpkg") |>
        select(id_data_source = snl_id, primary_commodity, commodities_list = list_of_commodities) |>
        mutate(data_source = "S&P")

    # Read mining properties from GEM
    # GEM data issues:
    #   - Some NAs are filled with "-"
    #   - Some entries of latitude includes both latitude and longitude separated by comma
    #   - The "GEM Mine ID" is not unique
    gem <- read_xlsx("./data/gem.xlsx", sheet = 2, na = c("-", "NA", "N/A")) |>
        select(id_data_source = `GEM Mine ID`, latitude = Latitude, longitude = Longitude) |>
        mutate(
            latitude = as.numeric(str_replace(latitude, ",.*", "")),
            longitude = as.numeric(str_replace(longitude, ",.*", "")))

    gem <- read_xlsx("./data/gem.xlsx", sheet = 3, na = c("-", "NA", "N/A")) |>
        select(id_data_source = `GEM Mine ID`, latitude = Latitude, longitude = Longitude) |>
        mutate(
            latitude = as.numeric(str_replace(latitude, ",.*", "")),
            longitude = as.numeric(str_replace(longitude, ",.*", ""))) |>
        bind_rows(gem) |>
        filter(!(is.na(latitude) | is.na(longitude))) |>
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326, sf_column_name = "geom") |>
        mutate(primary_commodity = "Coal", commodities_list = "Coal", data_source = "GEM - Coal Mine Tracker")

    # Merge mining properties
    mining_properties <- bind_rows(jasansky, sp) |>
        bind_rows(gem) |>
        mutate(id = str_c("P", str_pad(row_number(), width = 7, pad = "0")), data_type = "property") |>
        select(id, id_data_source, primary_commodity, commodities_list, data_type, data_source, geom) |>
        filter(!st_is_empty(geom))

    st_write(mining_properties, dsn = "./data/mining_properties.gpkg")

} else {
   mining_properties <- st_read(dsn = "./data/mining_properties.gpkg")
}

# Create cluster data
if (!file.exists("./data/cluster_data.gpkg")) {

    # Deactivate S2 for st_nearest_feature operations for invalid geometries
    sf_use_s2(FALSE)

    # Read biomes
    ecoregions <- st_read("./data/ecoregions/ecoregions.gpkg")

    # Read countries
    world_map <- ne_countries(scale = "medium", returnclass = "sf") |>
        select(country_name = admin, country_isoa3 = adm0_a3)

    # Create cluster data including polygons and points
    cluster_data <- bind_rows(mining_land_use, mining_properties)

    cluster_data <- st_centroid(cluster_data) |>
        st_join(world_map, join = st_nearest_feature) |>
        st_join(ecoregions, join = st_nearest_feature) |>
        mutate(centroid = geom, geom = cluster_data$geom)

    st_write(cluster_data, dsn = "./data/cluster_data.gpkg")

    sf_use_s2(TRUE)

} else {

   cluster_data <- st_read(dsn = "./data/cluster_data.gpkg")

}

# Data checks
nrow(cluster_data)
sum(cluster_data$area_mine, na.rm = TRUE)

st_drop_geometry(cluster_data) |>
    group_by(biome_name) |>
    summarise(area_mine = sum(area_mine, na.rm = TRUE)) |>
    mutate(perc = area_mine / sum(perc = area_mine)) |>
    arrange(desc(perc))

st_drop_geometry(cluster_data) |>
    group_by(country_name) |>
    summarise(area_mine = sum(area_mine, na.rm = TRUE)) |>
    mutate(perc = area_mine / sum(perc = area_mine)) |>
    arrange(desc(perc))

