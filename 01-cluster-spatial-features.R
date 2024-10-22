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
library(Matrix)
source("./R/comp_dist_matrix.R")
source("./R/collapse_groups.R")

cluster_data_path <- "./data/cluster_data.gpkg"
cluster_data_layer <- "cluster_data"
cluster_data_split_at = "id_batch"
dist_matrix_dir <- "./data/dist_matrix"
hcluster_results_path = "./data/hcluster"
h <- units::set_units(seq(1, 20, 1), km)

# ------------------------------------------------------------------------------
# get batch list
batch_list <- 
  st_read(dsn = cluster_data_path, quiet = TRUE, query = str_c("SELECT ",cluster_data_split_at," FROM ", cluster_data_layer)) |> 
  as_tibble() |> 
  distinct() |> 
  arrange(id_batch) |>
  drop_na() |>
  unlist()

# ------------------------------------------------------------------------------
# set cluster 
cl <- makeCluster(parallel::detectCores(), type = "FORK")
registerDoParallel(cl)

# ------------------------------------------------------------------------------
# calculate distance matrix per batch in parallel
pb <- progress_bar$new(
  format = "  calculating distance matrix [:bar] :percent in :elapsed",
  total = length(batch_list), clear = FALSE, width= 60)
dir.create(dist_matrix_dir, showWarnings = FALSE)
dist_files <- foreach(
  mine_split = batch_list, 
  .combine = 'c'
) %do% {
  comp_dist_matrix(
    id_batch = mine_split,
    in_path = cluster_data_path,
    layer = cluster_data_layer,
    split_att = cluster_data_split_at,
    output_dir = dist_matrix_dir,
    pb = pb)
}

# ------------------------------------------------------------------------------
# hcluster
dir.create(hcluster_results_path, showWarnings = FALSE)
hc_file_list <- foreach(
  id_batch = batch_list,
  .combine = 'c'
) %do% {
  
  cat("Processing batch",id_batch, "\n")
  hc_file <- str_glue("{hcluster_results_path}/batch_{stringr::str_pad(id_batch, width = 4, pad = 0)}.csv")

  if(!file.exists(hc_file)){

    out <- st_read(dsn = cluster_data_path, 
                   query = str_c("SELECT id, id_group FROM ",cluster_data_layer," WHERE ",cluster_data_split_at," = '", id_batch, "'"), quiet = TRUE) |>
      as_tibble() 
    
    # Compute cluster ids
    if(nrow(out) > 1){
      
      cat("    reading dist matrix...", "\n")
      mat <- readRDS(stringr::str_glue("{dist_matrix_dir}/batch_{stringr::str_pad(id_batch, width = 4, pad = 0)}.rds"))
      
      cat("    loop over all h levels...", "\n")
      cluster_ids <- foreach(
        k = h,
        .combine = 'left_join'
      ) %do% {
        cat("    computing hcluster with h ", k, "\n")
        do.call("bind_rows", lapply(unique(out$id_group), function(g){
          g_ids <- sort(out$id[out$id_group==g])
          g_mat <- t(mat[g_ids,g_ids])
          diag(g_mat) <- 0
          if(nrow(g_mat)==1) return(tibble(id = g_ids, id_group = g, cl = 1))
          dist_mat <- structure(g_mat@x, Size = length(g_ids), class = "dist", Labels = g_ids, Diag = FALSE, Upper = FALSE)
          cl <- fastcluster::hclust(dist_mat, method = "single") |> 
            cutree(h = as.numeric(units::set_units(k, m))) # dist matrix are in metres
          tibble(id = g_ids, id_group = g, cl)
        })) |>
          group_by(id_group, cl) |>
          mutate(!!sym(str_c("id_hc_", k)) := cur_group_id()) |>
          ungroup() |>
          select(-cl)
      }

    } else {
      cat("    add single hcluster...", "\n")
      cluster_ids <- tibble(id = as.character(rep(1, length(h))), col_name = str_c("id_hc_", h)) |>
        pivot_wider(values_from = id, names_from = col_name) |>
        mutate(id = out$id, id_group = out$id_group, .before = 1)
    }
    
    cat("    writing hcluster results to",hc_file, "\n")
    left_join(out, cluster_ids, by = join_by("id", "id_group")) |>
      write_csv(file = hc_file)
    
  } else {
    cat("    skipping existing file", hc_file, "\n")
  }
  
  cat("Done", "\n")

  return(hc_file)
  
}

cluster_ids <- list.files(hcluster_results_path, pattern = "\\.csv$", full.names = TRUE) |>
  lapply(read_csv)

cluster_ids <- do.call(rbind, cluster_ids)
rownames(cluster_ids) <- NULL

cluster_ids <- cluster_ids |>
  left_join(
    st_read(
      dsn = cluster_data_path,
      query = str_c("SELECT id, id_batch, id_group, primary_commodity, commodities_list, area_mine FROM cluster_data"), quiet = TRUE)
  )

# Creat unique cluster id and group commodities
id_hcluster_cols <- names(cluster_ids) |>
  str_subset("^id_hc_")
names(id_hcluster_cols) <- id_hcluster_cols
hcluster_concordance <- foreach(
  col_name = id_hcluster_cols, 
  .combine = 'left_join'
) %dopar% {
  cluster_ids |> 
    group_by(id_batch, id_group, !!sym(col_name)) |> 
    transmute(id,
              !!sym(col_name) := str_c("H", str_pad(cur_group_id(), pad = "0", width = 7)),
              !!sym(str_c("primary_", str_remove_all(col_name, "id_"))) := collapse_groups(primary_commodity),
              !!sym(str_c("list_", str_remove_all(col_name, "id_"))) := collapse_groups(commodities_list)
              ) |>
    ungroup()
}

write_csv(hcluster_concordance, str_c("./data/hcluster_concordance.csv"))

