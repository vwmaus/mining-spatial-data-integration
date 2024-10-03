library(sf)
library(s2)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(rnaturalearth)
library(rnaturalearthdata)

dir.create("./data", showWarnings = F)

# Import fast version of S2 union split agg
source("s2_union_split_agg.R")

###### Download data sources

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
    sf_use_s2(FALSE) # set FALSE to perform geometry fixing operations
    st_read("./data/ecoregions/Ecoregions2017.shp") |>
        select(ecoregions_name = ECO_NAME, biome_name = BIOME_NAME) |>
        st_cast(to = "POLYGON") |>
        st_simplify() |>
        st_write(dsn = "./data/ecoregions/ecoregions.gpkg")
    sf_use_s2(TRUE)
}

if(!file.exists("./data/tang/tang.gpkg")){
    download.file("https://zenodo.org/api/records/7894216/files-archive", destfile = "./data/tang.zip")
    unzip("./data/tang.zip", exdir = "./data/tang")
    sf_use_s2(FALSE) # set FALSE to perform geometry fixing operations
    st_read("./data/tang/74548_projected\ polygons.shp") |>
        st_transform(crs = 4326) |>
        select(geom = geometry) |>
        st_cast(to = "POLYGON") |>
        st_make_valid() |>
        st_buffer(0) |>
        st_simplify(dTolerance = 0.0001) |>
        filter(!st_is_empty(geom)) |>
        st_cast("POLYGON") |>
        st_make_valid() |>
        mutate(tang = "Tang & Werner (2023)") |>
        st_write(dsn = "./data/tang/tang.gpkg", delete_dsn = TRUE)
    sf_use_s2(TRUE)
}

if(!file.exists("./data/maus/maus.gpkg")){
    dir.create("./data/maus", showWarnings = FALSE)
    download.file("https://download.pangaea.de/dataset/942325/files/global_mining_polygons_v2.gpkg", destfile = "./data/maus/global_mining_polygons_v2.gpkg")
    sf_use_s2(FALSE) # set FALSE to perform geometry fixing operations
    st_read("./data/maus/global_mining_polygons_v2.gpkg") |>
        bind_rows(st_read("./data/maus/maus_new_polygons.gpkg")) |>
        transmute(maus = "Maus et al. (2022)") |>
        st_transform(crs = 4326) |>
        st_write(dsn = "./data/maus/maus.gpkg", delete_dsn = TRUE)
    sf_use_s2(TRUE)
}

if(!file.exists("./data/osm/osm.gpkg")){
    # Requires OSM data osm_quarry_check_20211125.gpkg
    sf_use_s2(FALSE) # set FALSE to perform geometry fixing operations
    st_read("./data/osm/osm_quarry_check_20211125.gpkg") |>
        st_transform(crs = 4326) |>
        transmute(osm = "OpenStreetMap") |>
        st_cast(to = "POLYGON") |>
        st_make_valid() |>
        st_buffer(0) |>
        st_simplify(dTolerance = 0.0001) |>
        filter(!st_is_empty(geom)) |>
        st_cast("POLYGON") |>
        st_make_valid() |>
        st_write(dsn = "./data/osm/osm.gpkg", delete_dsn = TRUE)
    sf_use_s2(TRUE)
}

# Integrate mining land use datasets and and remove overlaps
if(!file.exists("./data/maus_tang_osm.gpkg")){

    # Read mining polygons from Maus et al. 2022 + new polygons
    maus <- st_read("./data/maus/maus.gpkg") |>
        select(geom)

    # Read mining polygons from Tang & Werner 2023
    tang <- st_read("./data/tang/tang.gpkg") |>
        select(geom)

    # Read mining polygons from OpenStreetMap
    osm <- st_read("./data/osm/osm.gpkg") |>
        select(geom)

    sf_use_s2(TRUE)
    mining_land_use <- bind_rows(maus, tang, osm) |>
        select(geom) |>
        #filter(s2_is_valid(geom)) |>
        st_as_s2() |>
        s2_union_split_agg(options = s2_options(model = "closed"), progress = TRUE) |>
        st_as_sf() |>
        select(geom = "geometry") |>
        filter(st_is(geom, c("POLYGON", "MULTIPOLYGON"))) |>
        st_cast("POLYGON") |>
        st_make_valid() |>
        #filter(st_is_valid(geom)) |>
        filter(!st_is_empty(geom)) |>
        st_cast("POLYGON")

    # Add attributes
    idx_maus <- lengths(st_intersects(mining_land_use, maus)) > 0
    idx_tang <- lengths(st_intersects(mining_land_use, tang)) > 0
    idx_osm <- lengths(st_intersects(mining_land_use, osm)) > 0
    data_source <- sapply(1:length(idx_maus), function(i) str_c(c("Maus et al. (2022)", "Tang & Werner 2023", "OpenStreetMap")[c(idx_maus[i],idx_tang[i],idx_osm[i])], collapse = ";"))
    mining_land_use <- mining_land_use |>
        mutate(
            id = str_c("A", str_pad(row_number(), width = 7, pad = "0")),
            area_mine = st_area(geom),
            data_type = "land-use",
            data_source = data_source
            ) |>
            select(id, data_type, data_source, area_mine, geom)

    st_write(mining_land_use, "./data/maus_tang_osm.gpkg", delete_dsn = TRUE)

} else {
   mining_land_use <- st_read("./data/maus_tang_osm.gpkg")
}

# Summary by data sources
group_by(st_drop_geometry(mining_land_use), data_source) |>
    summarise(area_mine = sum(area_mine)) |>
    mutate(perc = area_mine / sum(area_mine)) |>
    arrange(desc(area_mine)) 

sum(mining_land_use$area_mine)

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

    # Deactivate S2 for st_nearest_feature operations because of invalid geometries in the external data sources
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
        mutate(geom = cluster_data$geom)

    st_write(cluster_data, dsn = "./data/cluster_data.gpkg", delete_dsn = TRUE)

    sf_use_s2(TRUE)

} else {

   cluster_data <- st_read(dsn = "./data/cluster_data.gpkg")

}

# Data checks
nrow(cluster_data)
sum(cluster_data$area_mine, na.rm = TRUE) * 1e-6

st_drop_geometry(cluster_data) |>
    group_by(biome_name) |>
    summarise(area_mine = sum(area_mine, na.rm = TRUE) * 1e-6) |>
    mutate(perc = area_mine / sum(perc = area_mine)) |>
    arrange(desc(perc))

st_drop_geometry(cluster_data) |>
    group_by(country_name) |>
    summarise(area_mine = sum(area_mine, na.rm = TRUE) * 1e-6) |>
    mutate(perc = area_mine / sum(perc = area_mine)) |>
    arrange(desc(perc))



