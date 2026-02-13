
rarefy_all_samples_iter <-
  
  function(n_iter, data_rarefy) {
  purrr::map(n_iter,  
    
    rarefied_data <- data_rarefy %>% 
      rarefy_all_samples(n_grains = 300),
    
    tibble(
      id = as.character(x), # create first column for id
      rarefied_dataset = list(rarefied_data) # Wraps data in a list-column
    )
  )
  }
