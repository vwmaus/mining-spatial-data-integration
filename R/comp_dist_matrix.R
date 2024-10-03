# ------------------------------------------------------------------------------
# declare function to compute distance matrices for each country
comp_dist_matrix <-

  function(x,
           in_path,
           split_att,
           layer,
           output_dir = ".",
           pb = NULL) {
    
    if(!is.null(pb)) pb$tick()
    
    gc()
    
    path_features_dist_meter <- 
      stringr::str_glue("{output_dir}/{x}.rds")
    
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    # stop processing if job has less than two features ------------------------
    x <- st_read(
      dsn = in_path, 
      query = str_c("SELECT id, geom FROM ", layer, " WHERE ", split_att, " = \"", x, "\""), 
      quiet = TRUE)
    
    if( nrow(x) < 2 ){
      return(NULL)
    }
    
    # compute geographical distance in parallel --------------------------------
    if(!file.exists(path_features_dist_meter)){
      
      ids <- x$id
      x <- sf::st_geometry(x)
      dist_matrix <- foreach(
        xi = split(x, 1:length(x)), 
        .combine = 'rbind'
      ) %dopar% {
        sf::st_distance(x = xi, y = x)
      }
      
      row.names(dist_matrix) <- ids
      saveRDS(as.dist(dist_matrix), file = path_features_dist_meter)
      
    }
    
    return(path_features_dist_meter)
    
}