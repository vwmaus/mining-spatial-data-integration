# ------------------------------------------------------------------------------
# declare function to compute distance matrices for each country
comp_dist_matrix <-
  
  function(id_batch,
           in_path,
           split_att,
           layer,
           output_dir = ".",
           pb = NULL) {
    
    if(!is.null(pb)) pb$tick()
    
    gc()
    
    cat("processing batch", id_batch, "\n")
    
    path_dist_matrix <- 
      stringr::str_glue("{output_dir}/batch_{stringr::str_pad(id_batch, width = 4, pad = 0)}.rds")
    
    cat("   setting batch output to path", path_dist_matrix, "\n")
    
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    cat("   loading batch data...", "\n")
    x <- st_read(
      dsn = in_path, 
      query = str_c("SELECT id, id_group, geom FROM ", layer, " WHERE ", split_att, " = \"", id_batch, "\""), 
      quiet = TRUE) |>
      mutate(n = row_number())
    
    if( nrow(x) < 2 ){
      return(NULL)
    }
    
    # compute geographical distance in parallel --------------------------------
    if(!file.exists(path_dist_matrix)){
      
      #x <- bind_rows(slice(filter(x, id_group == ids_group[1]), 1:7), slice(filter(x, id_group == ids_group[2]), 1:7)) |> mutate(n = row_number())
      cat("   splitting batch data for each group...", "\n")
      ids <- x$id
      ids_group <- unique(x$id_group)
      y <- split(x, x$id)[ids]
      
      cat("   processing batch groups...", "\n")
      dist_values <- foreach(
        xi = y,
        .combine = 'c'
      ) %dopar% {
        xj <- filter(x, id_group == xi$id_group & n >= xi$n)
        as.numeric(sf::st_distance(x = xi, y = xj)[1,])
      }
      
      cat("   retrieving matrix indices...", "\n")
      idx = do.call("c", lapply(x$n, function(i) rep(i, length(x$n))[x$n>=i & x$id_group == x$id_group[i]]))
      jdx = do.call("c", lapply(x$n, function(i) x$n[x$n>=i & x$id_group == x$id_group[i]]))
      
      cat("   creating a sparse matrix...", "\n")
      sparse_dist_matrix <- sparseMatrix(
        i = idx,
        j = jdx,
        x = dist_values,
        dims = c(nrow(x), nrow(x)),
        dimnames = list(x$id, x$id))
      
      cat("   compressing matrix...", "\n")
      sparse_dist_matrix <- as(sparse_dist_matrix, "dgCMatrix")
      
      cat("   writing results to", path_dist_matrix, "\n")
      saveRDS(sparse_dist_matrix, file = path_dist_matrix)
      
    }
    
    cat("Done!", "\n")
    
    return(path_dist_matrix)
    
  }