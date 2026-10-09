
## RAREFIED

purrr::walk(
  .x = seq_along(vec_names_rarefied_study3),
  .f = ~ {
    
    dir_path <- 
      here::here(
        "Data/Paper_1/data_supplementary/study3/rarefied"
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
    
    data_temp_rarefied <- 
      vec_names_rarefied_study3[[.x]] %>% 
      readr::read_rds() %>% 
      dplyr::mutate(
        dataset_id = stringr::str_extract(dataset_id_age , "^[^_]+"),
        .before = dataset_id_age
      ) %>% 
      dplyr::rename(sample_id = dataset_id_age) %>% 
      left_join(data_region, by = "dataset_id")
    
    n_datasets <- 
      data_temp_rarefied %>% 
      get_number_of_datasets(group_var = "region", name = "rarefied") %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    assertthat::assert_that(
      is.data.frame(n_datasets),
      nrow(n_datasets) > 0,
      "n" %in%  names(n_datasets) 
    )
    
    n_samples <- 
      data_temp_rarefied %>% 
      get_number_of_samples(name = "rarefied", group_var = "region")  %>% 
      rlang::set_names(
        nm = c("region", "n", "step")
      )
    
    
    assertthat::assert_that(
      is.data.frame(n_samples),
      nrow(n_samples) > 0,
      "n" %in%  names(n_samples) 
    )
    
    n_taxa <- 
      data_temp_rarefied %>%
      tidyr::pivot_longer(
        col = -c(sample_id, dataset_id,region),
        names_to = "taxa", values_to = "value"
      ) %>%
      select(-region) %>% 
      left_join(all_taxa_harm, by = "taxa") %>% 
      get_number_of_taxa(name = "rarefied", group_var = "region") %>% 
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
      relocate(step) %>% 
      dplyr::left_join(
        n_taxa %>% 
          dplyr::rename(
            n_taxa = n
          ),
        by = join_by(region, step),
      ) 
    
    write_csv(
      x = data_overview_one_iteration,
      file = file_name
    )
    
  },
  .progress = TRUE)


# Check one iteration

data_overview_one_iter_rarefied <-
  read_csv(here("Data/Paper_1/data_supplementary/study3/rarefied/1000.csv"))


