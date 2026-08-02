
samples <-  
  data_harmonised_study3 %>% 
  distinct(dataset_id,sample_id)

taxa <- data_harmonised_study3 %>% 
  distinct(dataset_id,taxa)
  

##RICHNESS ESTIMATION

purrr::walk(
  .progress = TRUE,
  .x = seq_along(vec_names_richness_study3),
  .f = ~ {
    
    dir_path <- 
      here::here(
        "Data/Paper_1/data_supplementary/study3/richness"
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
    
    data_temp_richness <- 
      vec_names_richness_study3[[1]] %>% 
      readr::read_rds() %>% 
      left_join(samples, by = "dataset_id") %>% 
      left_join(taxa, by = "dataset_id")
    
    n_datasets <- 
      data_temp_richness %>% 
      get_number_of_datasets(group_var = "region", name = "richness") %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    assertthat::assert_that(
      is.data.frame(n_datasets),
      nrow(n_datasets) > 0,
      "n" %in%  names(n_datasets) 
    )
    
    
    n_samples <- 
      data_temp_richness %>% 
      get_number_of_samples(name = "richness", group_var = "region")  %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    
    assertthat::assert_that(
      is.data.frame(n_samples),
      nrow(n_samples) > 0,
      "n" %in%  names(n_samples) 
    )
    
    
    n_taxa <- 
      data_temp_richness %>%
      get_number_of_taxa(name = "richness", group_var = "region") %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    
    assertthat::assert_that(
      is.data.frame(n_taxa),
      nrow(n_taxa) > 0,
      "n" %in%  names(n_taxa) 
    )
    
    data_overview_one_iteration <- 
      n_datasets %>% 
      dplyr::left_join(
        n_samples,
        by = join_by(region, step),
        suffix = c("_datasets", "_samples")
      ) %>% 
      dplyr::left_join(
        n_taxa %>% 
          dplyr::rename(
            n_taxa = n
          ),
        by = join_by(region, step),
      ) %>% 
      relocate(step, .before = n_datasets)
    
    write_csv(
      x = data_overview_one_iteration,
      file = file_name
    )
    
  })

# Check one iteration

data_overview_one_iter_data_temp_richness <-
  read_csv(here("Data/Paper_1/data_supplementary/study3/richness/995.csv"))




