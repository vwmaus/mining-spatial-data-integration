library(stringr)
library(dplyr)
library(sf)
library(progress)
library(tibble)
library(readr)
library(tidyr)
library(fastcluster)
library(parallel)
library(foreach)
library(doParallel)
library(ggplot2)
library(scales)
source("./R/comp_dist_matrix.R")
source("./R/collapse_groups.R")

dir.create("./output", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# set cluster 
cl <- makeCluster(parallel::detectCores(), type = "FORK")
registerDoParallel(cl)

# ------------------------------------------------------------------------------
# split data per country
isoa3_list <- 
  st_read(dsn = "./data/cluster_data.gpkg", quiet = TRUE,
          query = "SELECT country_isoa3 FROM cluster_data") |> 
  as_tibble() |> 
  distinct() |> 
  arrange(country_isoa3) |>
  drop_na()

# ------------------------------------------------------------------------------
# calculate distance matrix per country in parallel
pb <- progress_bar$new(
  format = "  calculating distance matrix [:bar] :percent in :elapsed",
  total = length(isoa3_list$country_isoa3), clear = FALSE, width= 60)
dist_files <- foreach(
  mine_split = isoa3_list$country_isoa3, 
  .combine = 'c'
) %do% {
  comp_dist_matrix(
    x = mine_split, 
    in_path = "./data/cluster_data.gpkg",
    layer = "cluster_data",
    split_att = "country_isoa3", 
    output_dir = "./data/dist_matrix",
    pb = pb)
}

# ------------------------------------------------------------------------------
# hcluster 
h <- units::set_units(seq(1, 20, 1), km)
dist_files <- dir("./data/dist_matrix", full.names = TRUE)
names(dist_files) <- stringr::str_remove_all(basename(dist_files), ".rds")
countries <- isoa3_list$country_isoa3
names(countries) <- isoa3_list$country_isoa3
mine_clusters <- foreach(
  f = countries, 
  .combine = 'bind_rows'
) %dopar% {

  # Read country features
  out <- 
    st_read(dsn = "./data/cluster_data.gpkg", 
            query = str_c("SELECT id, country_isoa3, country_name, area_mine, commodities_list FROM cluster_data WHERE country_isoa3 = '", f, "'"), 
            quiet = TRUE) |>
    as_tibble()

  # Compute cluster ids
  if(nrow(out) > 1){
    dist_mat <- readRDS(dist_files[[f]])
    cluster_ids <- sapply(h, function(k){
      dist_mat |> 
        fastcluster::hclust(method = "single") |> 
        cutree(h = as.numeric(units::set_units(k, m))) # dist matrix are in metres
    })
    cluster_ids <- as_tibble(cbind(str_sub(row.names(cluster_ids), start = 1L, end = 8L), cluster_ids), .name_repair = ~ c("id", str_c("id_hcluster_", h)))
  } else {
    cluster_ids <- tibble(id = as.character(rep(1, length(h))), col_name = str_c("id_hcluster_", h)) |>
      pivot_wider(values_from = id, names_from = col_name) |>
      mutate(id = out$id, .before = 1)
  }

  return(left_join(out, cluster_ids, by = join_by("id")))

}

write_csv(mine_clusters, file = str_c("./data/hcluster_results.csv"))

# Get a list of all the id_hcluster columns (assuming they follow a similar naming pattern)
id_hcluster_cols <- names(mine_clusters) |>
  str_subset("^id_hcluster_")
names(id_hcluster_cols) <- id_hcluster_cols
hcluster_concordance <- foreach(
  f = id_hcluster_cols, 
  .combine = 'left_join'
) %dopar% {
  mine_clusters |> 
    group_by(country_isoa3, !!sym(f)) |> 
    transmute(
      id,
      !!sym(f) := str_c("H", str_pad(cur_group_id(), pad = "0", width = 7)),
      !!sym(str_c("comm_", str_remove_all(f, "id_"))) := collapse_groups(commodities_list)) |>
    ungroup() |>
    filter(str_detect(id, "A"))
}

# Check area
sum(mine_clusters$area_mine, na.rm = TRUE) == sum(full_join(select(mine_clusters, id, country_isoa3, area_mine), hcluster_concordance)$area_mine, na.rm = TRUE)
write_csv(hcluster_concordance, str_c("./data/mine_clusters_concordance.csv"))

red_d <- "#cc3e5b"
green_d <- "#306056"
yellow_d <- "#f6ae2d"

materials_area <- hcluster_concordance |> 
  left_join(select(mine_clusters, id, area_mine)) |>
  transmute(id, area_mine,
    across(all_of(starts_with("comm_")), ~ str_count(.x, ",") + 1, .names = "count{.col}"),
    across(all_of(starts_with("comm_")), is.na)) |>
  pivot_longer(all_of(contains("comm_")), names_to = c("var", "clust", "clust_dist"), values_to = "value", names_sep = "_") |>
  select(-clust) |>
  mutate(clust_dist = as.numeric(clust_dist)) |>
  pivot_wider(names_from = var, values_from = value) |>
  rename(count = countcomm, unknown = comm) |>
  #mutate(count = ifelse(is.na(count), 1, count)) |>
  group_by(clust_dist) |>
  summarise(perc_known = sum(area_mine * !unknown) / sum(area_mine), count_area = sum(area_mine / count, na.rm = TRUE) / sum(area_mine)) |>
  pivot_longer(-clust_dist)

max_unmixed_share <- materials_area |>
  pivot_wider(names_from = name, values_from = value) |>
  reframe(x1 = c(0, clust_dist[which.max(count_area)], 0, 0, 14, 0),
          x2 = c(rep(clust_dist[which.max(count_area)], 3), rep(14, 3)),
          y1 = c(perc_known[which.max(count_area)], count_area[which.min(count_area)], max(count_area), perc_known[clust_dist == 14], count_area[which.min(count_area)], count_area[clust_dist == 14]),
          y2 = c(rep(perc_known[which.max(count_area)], 2), max(count_area), rep(perc_known[clust_dist == 14], 2), count_area[clust_dist == 14]),
          count_area = c(rep(max(count_area), 3), rep(count_area[clust_dist == 14], 3))
          )

gp <- materials_area |>
  mutate(name = factor(name, levels = c("perc_known", "count_area"), labels = c("At least one commodity known", "Mining area per number of linked materials"))) |>
  ggplot(aes(x = clust_dist, y = value, colour = name)) +
    geom_line(linewidth = 1) +
    geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), data = max_unmixed_share, linewidth = 0.5, linetype = "dashed", color = red_d) +
    geom_text(aes(x = x2, y = min(y1), label = round(x2,0)), 
              data = max_unmixed_share, vjust = 1.5, hjust = -0.1, color = "black") +
    geom_text(aes(x = 0, y = y2, label = str_c(round(y2*100, 0), "%")), 
              data = max_unmixed_share, vjust = 0.5, hjust = 1.2, color = "black") +
    theme_minimal() +
    scale_color_manual(values = c(yellow_d, green_d)) +
    theme(
      legend.position = "inside",
      legend.position.inside = c(.85, .5)
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(color = "Area linked to material",
         x = "Cluster threshold distance (km)", 
         y = "Percentage of the total mining area")

ggsave(filename = str_c("./output/fig-hcluster-threshold.png"), plot = gp, bg = "#ffffff",
       width = 345, height = 140, units = "mm", scale = 1)
