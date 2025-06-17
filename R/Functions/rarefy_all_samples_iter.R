rarefy_all_samples_iter <- function(data_source, n_grains, n_iter){
  
  all_samples <- unique(data_source$sample_id)
  
  # Apply rarefaction to all samples
  results_list <- map(
    .x = all_samples,
    .f = ~ rarefy_pollen_grains_samp_iter(
      data_source = data_source,
      sample_id = .x,
      n_grains = n_grains,
      n_iter = n_iter),
    .progress = "TRUE")
  
  results <- dplyr::bind_rows(results_list) %>% 
    as_tibble()
  
  return(results)
}















