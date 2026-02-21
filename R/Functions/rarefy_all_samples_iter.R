
rarefy_all_samples_iter <- 
  
  function(n_iter, data_to_rarefy){
  
  n_iter <- 1:n_iter

  purrr::map(n_iter, function(x) {
    
    rarefied_data <- data_to_rarefy %>% 
      rarefy_all_samples(n_grains = 300) 
    
    tibble(
      id = as.character(x), # Ensures ID is character
      rarefied_dataset = list(rarefied_data) # Wraps data in a list-column
    ) 
  }) %>% purrr::list_rbind()
}
    
