
get_potential_ages <- 
  
  function(data, id){
    
    
    age_un_dataset_id <- data %>%
      filter(dataset_id == id ) %>% 
      unnest(age_uncertainty)
    
    # as data frame to a tibble
    age_un_dataset_id_tib <-
      as_tibble(do.call(data.frame,age_un_dataset_id ))
    
    #fix col names and create data frame with three cols(dataset_id, sample_id, potential_age)
    
    return(age_un_dataset_id_tib)
    
    dataset_potential_ages <-
      rename_with(age_un_dataset_id_tib,
                  ~ str_remove_all(.x,"[age_uncertainty.]")) %>% 
      rename(dataset.id = dsd)
    
    return(dataset_potential_ages)

     #rename cols to sample id
    dataset_potential_ages_long <-    
      dataset_potential_ages %>%
      pivot_longer(!dataset.id, names_to = "sample_id", values_to = "potential_age")
    
    return(dataset_potential_ages_long)
    
  }

