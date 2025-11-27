rarefy_all_samples <- function(data_source, n_grains) {
  # we expect that the data looks like
  # dataset_id, samples, ...
  
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_to_harmonize has to be data.frame"
  )
  
  assertthat::assert_that(
    all(c("dataset_id", "age") %in% colnames(data_source)),
    msg = "data must contain columns 'dataset_id', 'age' "
  )
  
  data_prepared <-
    data_source %>%
    dplyr::mutate(
      dataset_id_age = paste0(dataset_id,"_", age)
    ) %>%
    dplyr::select(-c(dataset_id,age)) %>% 
    dplyr::mutate(
      dplyr::across(-dataset_id_age, ~ tidyr::replace_na(.,0))
    ) %>% 
    tibble::column_to_rownames("dataset_id_age")
  
  
  results <-
    vegan::rrarefy(data_prepared, sample = n_grains) %>%
    as_tibble(rownames = "dataset_id_age") %>% # convert matrix to data frame
    dplyr::mutate(
      dataset_id = stringr::str_subset(dataset_id_age, "__", negate = TRUE),
      age = stringr::str_remove(dataset_id_age, ".*__")
    ) %>% 
    select(-c(dataset_id, age))
  
  return(results)
}