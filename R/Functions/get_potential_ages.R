
get_potential_ages <- 
  
  function(data_source){
    
    data_uncertainty_potential_ages <-
      data_source %>%
      mutate(
        age_uncertainty = purrr::map(age_uncertainty, ~ {
        # Convert the matrix to a long data frame
        as.data.frame(.x) %>%
          pivot_longer(cols = everything(), 
                       names_to = "sample_id", 
                       values_to = "potential_age")
      })) %>%
      unnest(age_uncertainty)
    
    return(data_uncertainty_potential_ages)
    
  }









