rarefy_study3 <- function(data_source, n_grains = 300) {
  
  # ... (your existing data_prepared code) ...
  
  # 1. Run the rarefaction
  rarefied_matrix <- vegan::rrarefy(data_source, sample = n_grains)
  
  # 2. Convert to tibble and FILTER based on the rarefaction outcome
  results <- rarefied_matrix %>% 
    as_tibble(rownames = "dataset_id_age") %>% 
    # FIX: Drop rows that failed to reach the required grain count (300)
    dplyr::filter(rowSums(across(where(is.numeric))) == n_grains) %>% 
    
    # Clean up names safely (replaces your str_subset bug)
    dplyr::mutate(
      dataset_id = stringr::str_remove(dataset_id_age, "_[^_]+$"),
      age = stringr::str_extract(dataset_id_age, "[^_]+$")
    )
  
  return(results)
}


