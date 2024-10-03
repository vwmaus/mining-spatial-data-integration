collapse_groups <- function(x){
  res <- glue::glue_collapse(glue::glue("{unique(x)}"), sep = ",", width = Inf) |> 
    str_split(pattern = ",") |> 
    unlist() |> 
    unique()
  res <- sort(res[res!="NA"])
  if(length(res)<1) return(NA)
  glue::glue_collapse(res, sep = ",", width = Inf)
}