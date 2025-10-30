library(sf)
library(s2)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(igraph)
library(units)
library(ggplot2)
library(viridis)
library(scales)

dir.create("./data", showWarnings = F)

# Import fast version of S2 union split agg
source("./R/s2_union_split_agg.R")

###### Download data sources

if(!dir.exists("./tmp/jasansky")){
    download.file("https://zenodo.org/records/7369478/files/open_database_mine_production.zip?download=1", destfile = "./tmp/jasansky.zip")
    unzip("./tmp/jasansky.zip", exdir = "./tmp/jasansky")
}

if(!file.exists("./tmp/gem.xlsx")){
    download.file("https://globalenergymonitor.org/wp-content/uploads/2024/04/Global-Coal-Mine-Tracker-April-2024.xlsx", destfile = "./tmp/gem.xlsx")
}

if(!file.exists("./tmp/tang/tang.gpkg")){
    download.file("https://zenodo.org/api/records/7894216/files-archive", destfile = "./tmp/tang.zip")
    unzip("./tmp/tang.zip", exdir = "./tmp/tang")
    sf_use_s2(FALSE) # set FALSE to perform geometry fixing operations
    st_read("./tmp/tang/74548_projected\ polygons.shp") |>
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
        st_write(dsn = "./tmp/tang/tang.gpkg", delete_dsn = TRUE)
    sf_use_s2(TRUE)
}

if(!file.exists("./tmp/maus/maus.gpkg")){
    dir.create("./tmp/maus", showWarnings = FALSE)
    download.file("https://download.pangaea.de/dataset/942325/files/global_mining_polygons_v2.gpkg", destfile = "./tmp/maus/global_mining_polygons_v2.gpkg")
    sf_use_s2(FALSE) # set FALSE to perform geometry fixing operations
    st_read("./tmp/maus/global_mining_polygons_v2.gpkg") |>
        bind_rows(st_read("./tmp/maus/maus_new_polygons.gpkg")) |>
        transmute(maus = "Maus et al. (2022)") |>
        st_transform(crs = 4326) |>
        st_write(dsn = "./tmp/maus/maus.gpkg", delete_dsn = TRUE)
    sf_use_s2(TRUE)
}

if(!file.exists("./tmp/osm/osm.gpkg")){
    # Requires OSM data osm_quarry_check_20211125.gpkg
    sf_use_s2(FALSE) # set FALSE to perform geometry fixing operations
    st_read("./tmp/osm/osm_quarry_check_20211125.gpkg") |>
        st_transform(crs = 4326) |>
        transmute(osm = "OpenStreetMap") |>
        st_cast(to = "POLYGON") |>
        st_make_valid() |>
        st_buffer(0) |>
        st_simplify(dTolerance = 0.0001) |>
        filter(!st_is_empty(geom)) |>
        st_cast("POLYGON") |>
        st_make_valid() |>
        st_write(dsn = "./tmp/osm/osm.gpkg", delete_dsn = TRUE)
    sf_use_s2(TRUE)
}

# Integrate mining land use datasets and and remove overlaps
if(!file.exists("./tmp/maus_tang_osm.gpkg")){

    # Read mining polygons from Maus et al. 2022 + new polygons
    maus <- st_read("./tmp/maus/maus.gpkg") |>
        select(geom)

    # Read mining polygons from Tang & Werner 2023
    tang <- st_read("./tmp/tang/tang.gpkg") |>
        select(geom)

    # Read mining polygons from OpenStreetMap
    osm <- st_read("./tmp/osm/osm.gpkg") |>
        select(geom)

    sf_use_s2(TRUE)
    
    mining_land_use <- bind_rows(maus, tang, osm) |>
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

    st_write(mining_land_use, "./tmp/maus_tang_osm.gpkg", delete_dsn = TRUE)

} else {
   mining_land_use <- st_read("./tmp/maus_tang_osm.gpkg")
}

# Summary by data sources
group_by(st_drop_geometry(mining_land_use), data_source) |>
    summarise(area_mine = sum(area_mine)) |>
    mutate(perc = area_mine / sum(area_mine)) |>
    arrange(desc(area_mine)) 

sum(mining_land_use$area_mine)

# Integrate mining properties
if (!file.exists("./tmp/mining_properties.gpkg")) {

    # Read mining properties from Jasansky et al. 2022
    jasansky <- st_read("./tmp/jasansky/data/facilities.gpkg") |>
        select(id_data_source = facility_id, primary_commodity, commodities_list = commodities_products) |>
        mutate(commodities_list = str_replace_all(commodities_list, ", ", ","), data_source = "Jasansky et al. 2022") |>
        filter(primary_commodity != "Processing") |>
        mutate(base_id = str_extract(id_data_source, "^[^.]+"), ending_number = as.integer(str_extract(id_data_source, "(?<=\\.)\\d+$"))) |>
        group_by(base_id) |>
        mutate(has_subentries = any(ending_number > 0)) |>
        ungroup() |>
        filter(!(ending_number == 0 & has_subentries)) |>
        select(-base_id, -ending_number, -has_subentries) |>
        st_cast("POINT") |>
        mutate(
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Gold"), "Gold", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Platinum"), "Platinum", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Manganese"), "Manganese", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "PGM"), "PGM", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Phosphate"), "Phosphate", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Nickel"), "Nickel", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Diamonds"), "Diamonds", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Bauxite"), "Bauxite", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Copper"), "Copper", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Lithium"), "Lithium", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Iron ore"), "Iron ore", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Molybdenum"), "Molybdenum", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Niobium"), "Niobium", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Limestone"), "Limestone", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Coal|coal"), "Coal", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine")&str_detect(commodities_list, "Zinc,Lead"), "Lead,Zinc", primary_commodity),
            primary_commodity = ifelse(str_detect(primary_commodity, "Other \\(poly\\)-metallic|Other mine"), commodities_list, primary_commodity))

    # Read mining properties from S&P
    sp <- st_read("./tmp/snl2020.gpkg", layer = "mining_commodities") |>
        select(id_data_source = snl_id, primary_commodity, commodities_list = list_of_commodities) |>
        mutate(data_source = "S&P") |>
        st_cast("POINT")

    # Read mining properties from GEM
    # GEM data issues:
    #   - Some NAs are filled with "-"
    #   - Some entries of latitude includes both latitude and longitude separated by comma
    #   - The "GEM Mine ID" is not unique
    gem <- read_xlsx("./tmp/gem.xlsx", sheet = 2, na = c("-", "NA", "N/A")) |>
        select(id_data_source = `GEM Mine ID`, latitude = Latitude, longitude = Longitude) |>
        mutate(
            latitude = as.numeric(str_replace(latitude, ",.*", "")),
            longitude = as.numeric(str_replace(longitude, ",.*", "")))

    gem <- read_xlsx("./tmp/gem.xlsx", sheet = 3, na = c("-", "NA", "N/A")) |>
        select(id_data_source = `GEM Mine ID`, latitude = Latitude, longitude = Longitude) |>
        mutate(
            latitude = as.numeric(str_replace(latitude, ",.*", "")),
            longitude = as.numeric(str_replace(longitude, ",.*", ""))) |>
        bind_rows(gem) |>
        filter(!(is.na(latitude) | is.na(longitude))) |>
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326, sf_column_name = "geom") |>
        mutate(primary_commodity = "Coal", commodities_list = "Coal", data_source = "GEM - Coal Mine Tracker") |>
        st_cast("POINT")

    # Merge mining properties
    mining_properties <- bind_rows(jasansky, sp) |>
        bind_rows(gem) |>
        mutate(id = str_c("P", str_pad(row_number(), width = 7, pad = "0")), data_type = "property") |>
        select(id, id_data_source, primary_commodity, commodities_list, data_type, data_source, geom) |>
        filter(!st_is_empty(geom))

    st_write(mining_properties, dsn = "./tmp/mining_properties.gpkg", delete_dsn = TRUE)

} else {
   mining_properties <- st_read(dsn = "./tmp/mining_properties.gpkg")
}

# Create cluster data
if (!file.exists("./tmp/cluster_data.gpkg")) {

    # Create cluster data including polygons and points
    cluster_data <- bind_rows(mining_land_use, mining_properties)

    # Use s2 geometry engine for accurate global distances
    sf_use_s2(TRUE)

    # Define the distance threshold
    distance_threshold <- set_units(20, "km")

    # Compute neighbor relationships using spatial indexing
    system.time(neighbors_list <- st_is_within_distance(cluster_data, dist = distance_threshold)) # (~3min)

    # Create an edge list
    edge_list <- do.call(rbind, lapply(seq_along(neighbors_list), function(i) {
    if (length(neighbors_list[[i]]) > 0) {
        cbind(i, neighbors_list[[i]])
    }}))

    # Remove self-loops and duplicate edges
    edge_list <- edge_list[edge_list[,1] != edge_list[,2], ]
    system.time(edge_list <- unique(t(apply(edge_list, 1, sort)))) # (~12min)

    # Build the graph
    g <- graph_from_edgelist(edge_list, directed = FALSE)

    # Find connected components
    components <- components(g)
    membership <- components$membership

    # Map feature indices to component IDs
    membership_df <- data.frame(
        feature_index = as.integer(V(g)),
        id_group = membership,
        id_batch = NA_integer_
    )

    # Assign id_group to cluster_data
    cluster_data <- mutate(cluster_data, id_group = NA_integer_, .before = geom)
    cluster_data$id_group[membership_df$feature_index] <- membership_df$id_group

    # Assign unique IDs to isolated features
    all_indices <- seq_len(nrow(cluster_data))
    isolated_indices <- setdiff(all_indices, membership_df$feature_index)
    if (length(isolated_indices) > 0) {
        max_id <- max(cluster_data$id_group, na.rm = TRUE)
        cluster_data$id_group[isolated_indices] <- seq(max_id + 1, max_id + length(isolated_indices))
    }

    # Set the maximum batch size for processing groups
    max_batch_size <- 10000

    # Initialize id_batch column
    id_group_batch <- cluster_data |>
        st_drop_geometry() |>
        group_by(id_group) |>
        reframe(group_size = n()) |>
        arrange(desc(group_size)) |>
        mutate(id_batch = NA_integer_, is_large_group = group_size > max_batch_size) |>
        mutate(
            id_batch = if_else(is_large_group, NA_integer_, 0L),
            cum_size = cumsum(if_else(is_large_group, 0L, group_size)),
            id_batch = if_else(is_large_group, row_number() + max(id_batch, na.rm = TRUE), ceiling(cum_size / max_batch_size))
        ) |>
        select(id_group, id_batch)

    cluster_data <- left_join(cluster_data, id_group_batch)

    st_write(cluster_data, dsn = "./tmp/cluster_data.gpkg", delete_dsn = TRUE)

} else {

   cluster_data <- st_read(dsn = "./tmp/cluster_data.gpkg")

}

# Verify the cluster data
cat("Number of features:", nrow(cluster_data), "\n")
cat("Mining area:", sum(cluster_data$area_mine, na.rm = TRUE) * 1e-6, "km2", "\n")
cat("Number of processing batches:", length(unique(cluster_data$id_batch)), "and", length(unique(cluster_data$id_group)), "groups:", "\n")
cat("Number of batches with NAs:", length(which(is.na(cluster_data$id_batch))))
cat("Summary of processing batch size:")
st_drop_geometry(cluster_data) |>
    group_by(id_batch) |>
    summarise(batch_size = n()) |>
    arrange(desc(batch_size)) |>
    select(batch_size) |>
    summary()

# join datasets and calculate shares
plot_data <- cluster_data |>
  filter(str_detect(id, "A")) |>
  transmute(Source = data_source, Area = st_area(geom) |> units::set_units("ha") |> units::drop_units(), Latitude = st_coordinates(st_centroid(geom))[,2]) |> 
  st_drop_geometry() |> 
  as_tibble() |> 
  mutate(Latitude = cut(Latitude, breaks = seq(-60, 90, 20))) |> 
  group_by(Source, Latitude) |> 
  summarise(Area = sum(Area, na.rm = TRUE))

gp <- plot_data |>
  mutate(Source = factor(Source, (plot_data |> arrange(Latitude, Area))$Source[1:7], (plot_data |> arrange(Latitude, Area))$Source[1:7])) |>
  ggplot(aes(x = Area, y = Latitude, fill = `Source`)) + 
  geom_bar(stat="identity", width = 0.8) + 
  theme_linedraw() + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.spacing = unit(0.0, "cm"),
        legend.key.size = unit(0.3, "cm")) + 
  scale_fill_viridis(discrete = TRUE, option = "D", direction = -1) +
  scale_x_continuous(labels = label_number(scale = 1e-6, accuracy = 0.1)) +
  xlab("Area (M ha)")
gp

ggsave(filename = str_c("./output/fig-spatial-distribution.png"), plot = gp, bg = "#ffffff",
       width = 345, height = 140, units = "mm", scale = 1)

