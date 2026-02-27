
rarefy_all_samples_iter <- 
  
  function(n_iter, data_to_rarefy){

  1:n_iter %>% 
  purrr::set_names() %>% 
  purrr::map(
    .progress = TRUE,
    .f = ~ rarefy_all_samples(data_to_rarefy, n_grains = 300)
    ) %>% 
  dplyr::bind_rows(.id = "id")
}
    

