library(sf)
library(dplyr)
library(stringr)
library(progress)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(knitr)
library(kableExtra)
library(stringr)

cluster_data_dir <- "./output/20241026-all_materials"

# Compare assigned commodities with Werner et al. (2020)
if(!file.exists("./tmp/werner/werner.gpkg")){

    dir.create("./tmp/werner/", showWarnings = FALSE, recursive = TRUE)
    download.file("https://data.mendeley.com/public-files/datasets/hbmmzjn3xn/files/daa70251-0d81-41ad-8a66-e418831271af/file_downloaded", destfile = "./tmp/werner/werner.kmz")
    download.file("https://data.mendeley.com/public-files/datasets/hbmmzjn3xn/files/38c45472-0490-4e63-a267-0842d63f6d18/file_downloaded", destfile = "./tmp/werner/werner.xlsx")
    
    werner_layers <- st_layers("./tmp/werner/werner.kmz")
    werner_layers <- werner_layers$name[werner_layers$features>0]
    werner_layers <- werner_layers[!werner_layers %in% as.character(2000:2020)]

    pb <- progress_bar$new(
        format = "  processing layers [:bar] :percent in :elapsed",
        total = length(werner_layers), clear = FALSE, width= 100)

    werner <- lapply(werner_layers, function(l){
      if(!is.null(pb)) pb$tick()
      st_read("./tmp/werner/werner.kmz", layer = l, quiet = TRUE) |>
        transmute(mine_name = l, infrastructure_type = Name) |>
        mutate(
            mine_name = str_remove_all(mine_name, "\\*"),
            year = str_extract(mine_name, "\\((\\d{4})\\)$") |> str_remove_all("\\(|\\)") |> as.numeric(),
            commodity = str_extract(mine_name, "\\(([^\\d]+)\\)") |> str_remove_all("\\(|\\)"),
            mine_name = str_remove_all(mine_name, "\\s*\\([^)]*\\)")) |>
        select(mine_name, infrastructure_type, year, commodity, geom = geometry) |>
        filter(!st_is_empty(geom), st_geometry_type(geom) != "POINT") |>
        st_make_valid() |>
        filter(st_is_valid(geom)) |>
        st_buffer(0) |>
        st_make_valid() |>
        st_simplify(dTolerance = 0.0001) |>
        st_make_valid()
    })

    werner <- bind_rows(werner) |>
        st_make_valid() |>
        st_buffer(0) |>
        st_make_valid() |>
        st_simplify(dTolerance = 0.0001) |>
        st_make_valid() 

    left_join(werner, read_csv("./tmp/werner/mines_name_concordance.csv")) |>  
    mutate(commodity = 
        ifelse(commodity %in% c("Elura", "AU", "Sally Malay", "Similkameen", "Snow Lake", "Vale", "South American Cu", "Minera Gaby",
                                "Don Gabriel-Papomono", "underground mine", "Cananea", "Francisco I. Madero", "Au-W", "Kurumbakari", "Bougainville",
                                "Northam", "North Am."), NA, commodity)) |>
        select(mine_name, mine_xlsx, country, infrastructure_type, year, commodity, commodities_list, primary_materials_list) |>
        st_write("./tmp/werner/werner.gpkg", delete_dsn = TRUE, layer = "werner")

}

check_werner_path <- str_c(cluster_data_dir, "/clusters_check_werner.csv")

if(!file.exists(check_werner_path)){

    source("./R/accuracy_metrics.R")

    werner <- st_read("./tmp/werner/werner.gpkg") |>
        mutate(ifelse(commodity == "Coal" & is.na(primary_materials_list), commodity, primary_materials_list)) |>
        select(mine_name, werner_primary_materials_list = primary_materials_list) |>
        group_by(mine_name) |>
        summarise(werner_primary_materials_list = unique(werner_primary_materials_list), geom = st_union(geom))

    sf_use_s2(FALSE)
    st_join(st_read(str_c(cluster_data_dir, "/mine_polygons.gpkg")), werner) |>
        st_drop_geometry() |>
        as_tibble() |>
        mutate(
            jaccard_primary = mapply(jaccard_similarity, primary_materials_list, werner_primary_materials_list),
            dice_primary = mapply(dice_coefficient, primary_materials_list, werner_primary_materials_list),
            overlapcoeff_primary = mapply(overlap_coefficient, primary_materials_list, werner_primary_materials_list),
            idoverlap_primary = overlapcoeff_primary > 0,
            jaccard_materials = mapply(jaccard_similarity, materials_list, werner_primary_materials_list),
            dice_materials = mapply(dice_coefficient, materials_list, werner_primary_materials_list),
            overlapcoeff_materials = mapply(overlap_coefficient, materials_list, werner_primary_materials_list),
            idoverlap_materials = overlapcoeff_materials > 0,
        ) |>
        write_csv(check_werner_path)

    sf_use_s2(TRUE)

}

mine_clusters_check <- read_csv(check_werner_path) |>
    drop_na(werner_primary_materials_list) |>
    mutate(
        overlapcoeff_primary = idoverlap_primary,
        idoverlap_primary = overlapcoeff_primary > 0,
        overlapcoeff_materials = idoverlap_materials,
        idoverlap_materials = overlapcoeff_materials > 0)

# View(st_drop_geometry(mine_clusters_check) |>select(primary_materials_list, werner_primary_materials_list, jaccard_primary, dice_primary, idoverlap_primary))
    
# Summary statistics of matching materials from Werner and Clustering results
summary_stats_primary <- mine_clusters_check |>
  reframe(

    n_full_match_primary = sum(jaccard_primary==1, na.rm = TRUE), # Perfect match
    n_partial_primary_present = sum(0<jaccard_primary & jaccard_primary<1, na.rm = TRUE), # Primary is present in the reference but not all reference are assigned
    n_full_primary_present = sum(overlapcoeff_primary==1, na.rm = TRUE), # The full set of primary is present in the reference
    n_no_match_primary = sum(overlapcoeff_primary==0 | is.na(overlapcoeff_primary)),

    p_full_match_primary = n_full_match_primary / n(),
    p_full_primary_present = n_full_primary_present / n(),
    p_partial_match_primary = n_partial_primary_present / n(),
    p_no_match_primary = n_no_match_primary / n(),

    mean_jaccard_primary = mean(jaccard_primary, na.rm = TRUE),
    median_jaccard_primary = median(jaccard_primary, na.rm = TRUE),
    sd_jaccard_primary = sd(jaccard_primary, na.rm = TRUE),
    
    mean_dice_primary = mean(dice_primary, na.rm = TRUE),
    median_dice_primary = median(dice_primary, na.rm = TRUE),
    sd_dice_primary = sd(dice_primary, na.rm = TRUE),

    mean_overlapcoeff_primary = mean(overlapcoeff_primary, na.rm = TRUE),
    median_overlapcoeff_primary = median(overlapcoeff_primary, na.rm = TRUE),
    sd_overlapcoeff_primary = sd(overlapcoeff_primary, na.rm = TRUE),

    n_idoverlap_primary = sum(idoverlap_primary, na.rm = TRUE),
    p_idoverlap_primary = n_idoverlap_primary / n(),
    mean_idoverlap_primary = mean(idoverlap_primary, na.rm = TRUE),
    median_idoverlap_primary = median(idoverlap_primary, na.rm = TRUE),
    sd_idoverlap_primary = sd(idoverlap_primary, na.rm = TRUE),

  )

summary_stats_primary |>
    pivot_longer(cols = everything(), names_to = "metric", values_to = "value") |>
    print(n = 30)


# Construct the table data
tbl <- tibble::tibble(
  Statistic = c("Count", "Proportion", "Mean", "SD", "Median"),

  `Jaccard` = c(
    "", "", sprintf("%.2f", summary_stats_primary$mean_jaccard_primary),
    sprintf("%.2f", summary_stats_primary$sd_jaccard_primary),
    sprintf("%.2f", summary_stats_primary$median_jaccard_primary)
  ),
  `Dice` = c(
    "", "", sprintf("%.2f", summary_stats_primary$mean_dice_primary),
    sprintf("%.2f", summary_stats_primary$sd_dice_primary),
    sprintf("%.2f", summary_stats_primary$median_dice_primary)
  ),
  `Overlap Coefficient` = c(
    "", "", sprintf("%.2f", summary_stats_primary$mean_overlapcoeff_primary),
    sprintf("%.2f", summary_stats_primary$sd_overlapcoeff_primary),
    sprintf("%.2f", summary_stats_primary$median_overlapcoeff_primary)
  ),
  `ID Overlap` = c(
    "", "", sprintf("%.2f", summary_stats_primary$mean_idoverlap_primary),
    sprintf("%.2f", summary_stats_primary$sd_idoverlap_primary),
    sprintf("%.2f", summary_stats_primary$median_idoverlap_primary)
  ),
  `Full Match (Primary)`     = c( # The full set of primary is present in the reference - not the other way arround
    summary_stats_primary$n_full_match_primary,
    sprintf("%.1f\\%%", 100 * summary_stats_primary$p_full_match_primary),
    "", "", ""
  ),
  `Partial Match (Primary)`  = c( # The some items in the primary set are present in the reference
    summary_stats_primary$n_partial_primary_present,
    sprintf("%.1f\\%%", 100 * summary_stats_primary$p_partial_match_primary),
    "", "", ""
  ),
  `No Match (Primary)`       = c(
    summary_stats_primary$n_no_match_primary,
    sprintf("%.1f\\%%", 100 * summary_stats_primary$p_no_match_primary),
    "", "", ""
  )
)[c(3,4,5,1,2),]

# Clean column names
names(tbl) <- str_replace_all(names(tbl), " \\(Primary\\)| \\(Materials\\)", "")

# Build the table
tbl |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", rep("r", ncol(tbl) - 1)),
    escape = FALSE,
    caption = "Summary of similarity metrics and match statistics based on the assigned primary commodities and the reference dataset by \\citet{werner_global-scale_2020}.\\label{tab:match-summary-transposed}"
  ) |>
  add_header_above(c(
    " " = 1,
    "Similarity Metrics" = 4,  # Jaccard + Dice + ID Overlap + match stats
    "Matching Counts" = 3
  )) |>
  kable_styling(
    latex_options = c("striped", "scale_down", "hold_position"),
    full_width = FALSE
  )  |> 
  writeLines(con = "./output/paper-supplementary/tbl-similarity-metrics.tex")


##### Data for Visual checks 
id_cluster_for_visual_check <- mine_clusters_check |>
    filter(idoverlap_materials<1) |>
    pull(id_cluster) |>
    unique()

mine_clusters_check |>
    filter(idoverlap_materials<1) |>
    select(id, id_cluster, primary_materials_list, werner_primary_materials_list, materials_list, jaccard_primary, dice_primary, idoverlap_primary) |>
    View()

data_visual_inspection <- mine_clusters_check |>
    select(id, id_cluster, werner_primary_materials_list, jaccard_primary, dice_primary, idoverlap_primary, jaccard_materials, dice_materials, idoverlap_materials) |>
    left_join(st_read(str_c(cluster_data_dir, "/cluster_features.gpkg"))) |>
    relocate(werner_primary_materials_list, .after = materials_list)

data_visual_inspection |>
    arrange(idoverlap_materials, id_cluster, id) 

st_read(str_c(cluster_data_dir, "/cluster_features.gpkg")) |>
    filter(id_cluster %in% id_cluster_for_visual_check) |>
    st_write(str_c(cluster_data_dir, "/mismatching_cluster_visual_check.gpkg"), delete_dsn = TRUE)


