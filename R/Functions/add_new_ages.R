
add_new_ages <- function(data_to_add_ages, path, file_name){
  
  data_with_new_ages <- 
    data_to_add_ages %>% 
    dplyr::mutate(
      data_with_new_age = purrr::map2(
        .progress = TRUE,
        .x = rarefied_data,
        .y = age_uncertainty,
        .f = ~ {
          
          data_pollen_nested <- 
            .x %>% 
            dplyr::mutate(
              dataset_id = str_extract(dataset_id_age, "^[^_]+"),
              .before = dplyr::everything()
            ) %>% 
            dplyr::select(-dataset_id_age) %>% 
            tidyr::nest(data_pollen = !dataset_id)
          
          data_age_nested <- 
            .y %>% 
            tidyr::nest(data_age = !dataset_id)
          
          
          dplyr::inner_join(
            data_pollen_nested,
            data_age_nested,
            by = "dataset_id"
          ) %>% 
            dplyr::mutate(
              data_with_new_age = purrr::map2(
                .x = data_pollen,
                .y = data_age,
                .f = ~ dplyr::bind_cols(.x, .y)
              ) )%>% 
            dplyr::select(dataset_id, data_with_new_age) %>% 
            tidyr::unnest(data_with_new_age) %>% 
            dplyr::relocate(sample_id, potential_age) %>% 
            dplyr::rename(age = potential_age)
          
        }
        readr::write_rds(data_with_new_ages,file = paste0(path,"/",file_name,".rds") )
      ) 
    )
  
  
}
 