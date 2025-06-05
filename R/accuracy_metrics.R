# Helper function to split comma-separated strings into sets
split_to_set <- function(x) {
  if (is.na(x)) return(character(0))  # Handle NA
  strsplit(x, ",\\s*")[[1]]  # Split by commas and remove leading/trailing spaces
}

# Jaccard Similarity
jaccard_similarity <- function(a, b) {
  set_a <- split_to_set(a)
  set_b <- split_to_set(b)
  intersect_len <- length(intersect(set_a, set_b))
  union_len <- length(union(set_a, set_b))
  if (union_len == 0) return(NA)  # Handle empty cases
  return(intersect_len / union_len)
}

# Dice Coefficient
dice_coefficient <- function(a, b) {
  set_a <- split_to_set(a)
  set_b <- split_to_set(b)
  intersect_len <- length(intersect(set_a, set_b))
  total_len <- length(set_a) + length(set_b)
  if (total_len == 0) return(NA)  # Handle empty cases
  return(2 * intersect_len / total_len)
}

# Overlap Coefficient function
overlap_coefficient <- function(a, b) {
  set_a <- split_to_set(a)
  set_b <- split_to_set(b)
  
  intersect_len <- length(intersect(set_a, set_b))
  
  if (min(length(set_a), length(set_b)) == 0) {
    return(NA)  # Handle cases where one of the lists is empty
  }
  
  return(intersect_len / min(length(set_a), length(set_b)))
}

identifier_overlap <- function(a, b) {
  as.integer(length(intersect(split_to_set(a), split_to_set(b))) > 0)
}
