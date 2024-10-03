s2_union_split_agg <- function(x, options = s2_options(), progress = FALSE){
  
  # check for intersects 
  idx_intersects <- s2::s2_intersects_matrix(x, x, options = options)
  
  # prapare groups of intersects 
  idx_list <- lapply(seq_along(idx_intersects)[lengths(idx_intersects) > 1], function(i) c(i, idx_intersects[[i]]))
  group_list <- tibble::tibble(fid = integer(), gid = integer())

  if(progress){
    require(progress)
    pb <- progress::progress_bar$new(format = "  calculating splits [:bar] :percent in :elapsed", total = length(idx_list), clear = FALSE, width = 60)
  }

  for(i in idx_list){
    if(progress) pb$tick()
    # check if intersecting fids already exist in the groups
    gids <- dplyr::filter(group_list, fid %in% unique(i))
    if(nrow(gids) > 0){
      # get for new fids 
      new_ids <- unique(i)[!unique(i) %in% group_list$fid]
      # get existing groups 
      new_gid <- unique(gids$gid)[1]
      # add new fids to existing groups
      group_list <- tibble::add_row(group_list, fid = new_ids, gid = new_gid) 
      # merge groups if new fids overlap several groups
      group_list <- dplyr::mutate(group_list, gid = ifelse(gid %in% gids$gid, new_gid, gid))
    } else {
      # create a new group id
      gid <- ifelse(nrow(group_list) == 0, 0, max(group_list$gid)) + 1
      # add all new fids to the new group
      group_list <- dplyr::bind_rows(tibble::tibble(fid = unique(i), gid = gid), group_list)
    }
  }
  
  # split features based on groups of intersects 
  x_split <- split(x[group_list$fid], group_list$gid)
  
  # spare features that do not intersect other features
  x <- x[-group_list$fid]
  
  # apply s2_union_agg to groups 
  if(progress){
    require(progress)
    pb <- progress::progress_bar$new(format = "  calculating union aggregate for each split [:bar] :percent in :elapsed", total = length(x_split), clear = FALSE, width = 60)
  }
  x_union <- lapply(x_split, function(x) {if(progress) pb$tick(); s2_union_agg(x, options = options)})
  
  # gather results into single object 
  result <- s2_rebuild_agg(c(x, do.call("c", x_union)), options = options)
  
  # # Optimized incremental aggregation using Reduce
  # result <- Reduce(
  #   function(acc, geom) s2_rebuild_agg(c(acc, geom), options = options),
  #   c(list(x), x_union)
  # )

  return(result)
  
}

