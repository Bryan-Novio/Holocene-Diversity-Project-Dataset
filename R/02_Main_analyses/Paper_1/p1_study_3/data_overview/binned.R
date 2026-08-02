##BINNED

samples <- pollen_data_study3 %>% distinct(dataset_id,sample_id)

purrr::walk(
  .progress = TRUE,
  .x = seq_along(vec_names_binned_study3),
  .f = ~ {
    
    dir_path <- 
      here::here(
        "Data/Paper_1/data_supplementary/study3/binned"
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
      
      return(NULL)
    } 
    
    data_temp_binned <- 
      vec_names_binned_study3 [[.x]]%>% 
      readr::read_rds() %>% 
      dplyr::left_join(samples, by = "dataset_id") %>% 
      dplyr::left_join(data_region, by = "dataset_id") 
    
    n_datasets <- 
      data_temp_binned %>% 
      get_number_of_datasets(group_var = "region", name = "binned") %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    assertthat::assert_that(
      is.data.frame(n_datasets),
      nrow(n_datasets) > 0,
      "n" %in%  names(n_datasets) 
    )
    
    
    n_samples <- 
      data_temp_binned %>% 
      get_number_of_samples(name = "binned", group_var = "region")  %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    
    assertthat::assert_that(
      is.data.frame(n_samples),
      nrow(n_samples) > 0,
      "n" %in%  names(n_samples) 
    )
    
    
    n_taxa <- 
      data_temp_binned %>%
      select(region, taxa) %>% 
      get_number_of_taxa(name = "binned", group_var = "region") %>% 
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
      dplyr::relocate(step)

    write_csv(
      x = data_overview_one_iteration,
      file = file_name
    )
    
    rm( data_temp_binned, n_datasets, n_samples, n_taxa, data_overview_one_iteration)
    gc()
    
  })


data_overview_one_iter_bin <-
  read_csv(here("Data/Paper_1/data_supplementary/study3/binned/1000.csv"))

