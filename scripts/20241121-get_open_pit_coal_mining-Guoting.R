library(sf)
library(dplyr)
library(readr)
library(readxl)


release_version_name <- "open_pit_coal"
release_version_date <- format(Sys.Date(), '%Y%m%d')
path_output <- str_c("./output/", release_version_date, "-", release_version_name)
dir.create(path_output, recursive = TRUE, showWarnings = FALSE)

    # Read mining properties from Jasansky et al. 2022
    jasansky <- st_read("./tmp/jasansky/data/facilities.gpkg") |>
        select(id_data_source = facility_id, primary_commodity, commodities_list = commodities_products, mine_type = facility_equipment) |>
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
        select(id_data_source = snl_id, primary_commodity, commodities_list = list_of_commodities, mine_type = mine_type_1) |>
        mutate(data_source = "S&P") |>
        st_cast("POINT") 

    # Read mining properties from GEM
    # GEM data issues:
    #   - Some NAs are filled with "-"
    #   - Some entries of latitude includes both latitude and longitude separated by comma
    #   - The "GEM Mine ID" is not unique
    gem <- read_xlsx("./tmp/gem.xlsx", sheet = 2, na = c("-", "NA", "N/A")) |>
        select(id_data_source = `GEM Mine ID`, latitude = Latitude, longitude = Longitude, mine_type = `Mine Type`) |>
        mutate(
            latitude = as.numeric(str_replace(latitude, ",.*", "")),
            longitude = as.numeric(str_replace(longitude, ",.*", "")))

    gem <- read_xlsx("./tmp/gem.xlsx", sheet = 3, na = c("-", "NA", "N/A")) |>
        select(id_data_source = `GEM Mine ID`, latitude = Latitude, longitude = Longitude, mine_type = `Mine Type`) |>
        mutate(
            latitude = as.numeric(str_replace(latitude, ",.*", "")),
            longitude = as.numeric(str_replace(longitude, ",.*", ""))) |>
        bind_rows(gem) |>
        filter(!(is.na(latitude) | is.na(longitude))) |>
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326, sf_column_name = "geom") |>
        mutate(primary_commodity = "Coal", commodities_list = "Coal", data_source = "GEM - Coal Mine Tracker") |>
        st_cast("POINT")

    # Merge mining properties
    open_pit_coal_mines <- bind_rows(jasansky, sp) |>
        bind_rows(gem) |>
        mutate(id = str_c("P", str_pad(row_number(), width = 7, pad = "0")), data_type = "property") |>
        st_drop_geometry() |>
        filter(primary_commodity == "Coal", str_detect(str_to_lower(mine_type), str_to_lower("Surface|Open Pit|Open Cut"))) |>
        select(id, id_data_source, primary_commodity, commodities_list, data_type, data_source)


mine_points <- st_read("./output/20241121-all_materials/mine_points.gpkg") |>
    filter(id %in% unique(open_pit_coal_mines$id)) |>
    st_drop_geometry() |>
    as_tibble()

mine_polygons <- st_read("./output/20241121-all_materials/mine_polygons.gpkg") |>
    filter(id_cluster %in% unique(mine_points$id_cluster))

st_write(mine_polygons, str_c(path_output, "/coal_pit_polygons.gpkg"), layer = "mine_polygons")
