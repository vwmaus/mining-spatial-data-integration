# Optimized function to harmonize materials
harmonize_materials <- function(materials_vector, mapping_table) {
  # Ensure mapping_table has the required columns
  required_cols <- c("material", "material_harmonized")
  if (!all(required_cols %in% colnames(mapping_table))) {
    stop("Mapping table must contain 'material' and 'material_harmonized' columns.")
  }
  
  # Prepare the mapping table:
  # - Trim whitespace
  # - Convert to lowercase for case-insensitive matching
  # - Ensure unique mappings by keeping the first occurrence
  mapping_table_prepared <- mapping_table |>
    mutate(
      material_clean = stri_trim_both(stri_trans_tolower(material)),
      material_harmonized_clean = stri_trim_both(material_harmonized)
    ) |>
    distinct(material_clean, .keep_all = TRUE)  # Remove duplicates, keep first
  
  # Create a named vector for lookup
  lookup <- setNames(mapping_table_prepared$material_harmonized_clean, 
                     mapping_table_prepared$material_clean)
  
  # Convert materials_vector to character (in case it's a factor or other type)
  materials_vector <- as.character(materials_vector)
  
  # Identify non-NA and non-empty entries
  valid_indices <- !is.na(materials_vector) & stri_trim_both(materials_vector) != ""
  
  # Initialize harmonized_vector with NA
  harmonized_vector <- rep(NA_character_, length(materials_vector))
  
  if (any(valid_indices)) {
    # Extract valid entries
    valid_materials <- materials_vector[valid_indices]
    
    # Split the materials by comma with optional surrounding whitespace
    # Using stringi for fast splitting
    split_list <- stri_split_fixed(valid_materials, ",")
    
    # Convert to lowercase and trim whitespace
    # Vectorizing operations for speed
    split_clean <- lapply(split_list, function(x) stri_trim_both(stri_trans_tolower(x)))
    
    # Harmonize materials using the lookup
    # Replace with harmonized names; materials not found will be NA
    harmonized_list <- lapply(split_clean, function(x) lookup[x])
    
    # Remove NA values (materials not found in the mapping table)
    harmonized_list <- lapply(harmonized_list, function(x) unique(x[!is.na(x)]))
    
    # Combine the harmonized materials back into a comma-separated string
    # If no materials remain, set to NA
    harmonized_strings <- sapply(harmonized_list, function(x) {
      if (length(x) == 0) {
        NA_character_
      } else {
        paste(x, collapse = ",")
      }
    }, USE.NAMES = FALSE)
    
    # Assign the harmonized strings back to the appropriate indices
    harmonized_vector[valid_indices] <- harmonized_strings
  }
  
  # Optionally, identify and log unmatched materials (for debugging)
  # This step is skipped in the optimized version for performance
  # If needed, it can be implemented separately
  
  return(harmonized_vector)
}
