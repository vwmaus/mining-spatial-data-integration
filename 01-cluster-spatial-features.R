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
# set cluster 
cl <- makeCluster(parallel::detectCores(), type = "FORK")
registerDoParallel(cl)

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
      
      cat("    preparing dist matrix...", "\n")
      dist_mat <- readRDS(stringr::str_glue("{dist_matrix_dir}/batch_{stringr::str_pad(id_batch, width = 4, pad = 0)}.rds"))
      # ii = which(out$id %in% slice(filter(out, id_group == unique(out$id_group)[1]), 1:7)$id)
      # jj = which(out$id %in% slice(filter(out, id_group == unique(out$id_group)[2]), 1:7)$id)
      # dist_mat <- dist_mat[c(ii,jj),c(ii,jj)]
      dist_mat = readRDS("/home/maus/Downloads/batch_0006.rds")
      dist_mat <- as.matrix(t(dist_mat))
      dist_mat[dist_mat == 0 & row(dist_mat) != col(dist_mat)] <- Inf
      dist_mat <- as.dist(dist_mat)
      
      cat("    computing hcluster for all h levels...", "\n")
      cluster_ids <- sapply(h, function(k){
        fastcluster::hclust(dist_mat, method = "single") |> 
          cutree(h = as.numeric(units::set_units(k, m))) # dist matrix are in metres
      })
      
      cat("    tidying hcluster results...", "\n")
      cluster_ids <- as_tibble(cbind(str_sub(row.names(cluster_ids), start = 1L, end = 8L), cluster_ids), .name_repair = ~ c("id", str_c("id_hc_", h)))
    } else {
      cat("    tidying hcluster results...", "\n")
      cluster_ids <- tibble(id = as.character(rep(1, length(h))), col_name = str_c("id_hc_", h)) |>
        pivot_wider(values_from = id, names_from = col_name) |>
        mutate(id = out$id, .before = 1)
    }
    
    cat("    writing hcluster results to",hc_file, "\n")
    left_join(out, cluster_ids, by = join_by("id")) |>
      write_csv(file = hc_file)
    
  } else {
    cat("    skiping existing file", hc_file, "\n")
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
              !!sym(str_c("primary_comm_list_", str_remove_all(col_name, "id_"))) := collapse_groups(primary_commodity),
              !!sym(str_c("comm_list_", str_remove_all(col_name, "id_"))) := collapse_groups(commodities_list)
              ) |>
    ungroup()
}

write_csv(hcluster_concordance, str_c("./data/hcluster_concordance.csv"))

