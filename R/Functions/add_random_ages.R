
add_random_ages <- function(id, data_assembly, data_p_ages){
  
  assembly_id <- 
    data_assembly %>%
    filter(dataset_id ==id)
  
  p_age_for_id <-
    data_p_ages %>%
    select(dataset_id, potential_age) %>% 
    filter(dataset_id == id)
  
  N_rand <- nrow(assembly_id)
  
  p_age_for_id_rand <-
    p_age_for_id  %>%
    dplyr::slice_sample(n = N_rand) %>% 
    select(potential_age)
  
    bind_cols(assembly_id,p_age_for_id_rand) %>% 
    relocate(potential_age)
  
}

