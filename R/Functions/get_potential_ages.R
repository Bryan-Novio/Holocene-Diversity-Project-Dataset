
get_potential_ages <- 
  
  function(data_source){
    
    data_uncertainty_potential_ages <-
      data_source %>%
      mutate(
        age_uncertainty = purrr::map(
          age_uncertainty, ~ {
        # Convert the matrix to a long data frame
        as.data.frame(.x) %>%
          tibble::rowid_to_column("id") %>% 
          pivot_longer(cols = !id,
                       names_to = "sample_id", 
                       values_to = "potential_age")
      })) %>%
      unnest(age_uncertainty)
    
    return(data_uncertainty_potential_ages)
    
  }

