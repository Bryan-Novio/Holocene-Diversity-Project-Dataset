

rarefy_all_samples <- function(data_source, n_grains){
  
  all_samples <- unique(data_source$sample_id)
  
  # Apply rarefaction to all samples
  results_list <- map(
    .x = all_samples,
    .f = ~ rarefy_one_sample(
      data_source_inner = data_source,
      sel_sample_id = .x,
      n_grains_inner = n_grains),
    .progress = "TRUE")
  
  results <- dplyr::bind_rows(results_list) %>% 
    as_tibble()
  
  return(results)
}
