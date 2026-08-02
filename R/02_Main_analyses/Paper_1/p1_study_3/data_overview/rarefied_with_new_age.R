##rarefied with new age

purrr::walk(
  .x = seq_along(vec_names_rarefied_study_new_age),
  .f = ~ {
    
    dir_path <- 
      here::here(
        "Data/Paper_1/data_supplementary/study3/rarefied_new_age"
      )
    
    dir.create(
      dir_path,
      showWarnings = FALSE,
      recursive = TRUE
      
    )
    
    
    file_name <- 
      stringr::str_glue(
        "{dir_path}/{.x}.csv"
      )
    
    
    if (
      file.exists(file_name)
    ) {
      
      return()
    } 
    
    data_temp_rarefied_new_age <- 
      vec_names_rarefied_study_new_age[[.x]] %>% 
      readr::read_rds() %>% 
      dplyr::left_join(data_region, by = "dataset_id") %>% 
      relocate(region)
    
    n_datasets <- 
      data_temp_rarefied_new_age %>% 
      get_number_of_datasets(group_var = "region", name = "rarefied_new_age") %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    assertthat::assert_that(
      is.data.frame(n_datasets),
      nrow(n_datasets) > 0,
      "n" %in%  names(n_datasets) 
    )
    
    
    n_samples <- 
      
      data_temp_rarefied_new_age %>% 
      get_number_of_samples(name = "rarefied_new_age", group_var = "region")  %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    
    assertthat::assert_that(
      is.data.frame(n_samples),
      nrow(n_samples) > 0,
      "n" %in%  names(n_samples) 
    )
    
    
    n_taxa <- 
      
      data_temp_rarefied_new_age %>%
      tidyr::pivot_longer(
        col = -c(sample_id, dataset_id, region, age),
        names_to = "taxa"
      ) %>% 
      get_number_of_taxa(name = "rarefied_new_age", group_var = "region") %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      ) %>% 
      dplyr::rename(
        n_taxa = n
      )
    
    
    assertthat::assert_that(
      is.data.frame(n_taxa),
      nrow(n_taxa) > 0,
      "n_taxa" %in%  names(n_taxa) 
    )
    
    data_overview_one_iteration <- 
      n_datasets %>% 
      dplyr::left_join(
        n_samples,
        by = join_by(region, step),
        suffix = c("_datasets", "_samples")
      ) %>% 
      relocate(step) %>% 
      dplyr::left_join(
        n_taxa,
        by = join_by(region,step),
      ) 
    
    write_csv(
      x = data_overview_one_iteration,
      file = file_name
    )
    
  },
  .progress = TRUE)

# Check one iteration 

data_overview_one_iter_rarefied_new_age <-
  read_csv(here("Data/Paper_1/data_supplementary/study3/rarefied_new_age/7.csv"))
