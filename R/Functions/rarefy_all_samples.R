rarefy_all_samples <- function(data_source, n_grains) {
  # we expect that the data looks like
  # dataset_id, samples, ...

  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_to_harmonize has to be data.frame"
  )

  assertthat::assert_that(
    all(c("dataset_id", "samples") %in% colnames(data_source)),
    msg = "data must contain columns 'dataset_id', 'samples' "
  )

  # samples is a nested data
  # sample_id,. taxa1, taxa2, ....
  assertthat::assert_that(
    all(c("sample_id") %in% colnames(data_source$samples[[1]])),
    msg = "Column `samples` must contain columns 'sample_id' "
  )

  data_prepared <-
    data_source %>%
    tidyr::unnest(samples) %>%
    dplyr::mutate(
      dataset_sample_id = paste0(dataset_id,"_", sample_id)
    ) %>%
    dplyr::mutate(
      dplyr::across(-dataset_sample_id, ~ tidyr::replace_na(.,0))
    ) %>% 
    tibble::column_to_rownames("dataset_sample_id")

  
  results <-
    vegan::rrarefy(data_prepared, sample = n_grains) %>%
    as_tibble(rownames = "dataset_sample_id") %>% # convert matrix to data frame
    dplyr::mutate(
      dataset_id = stringr::str_subset(dataset_sample_id, "__", negate = TRUE),
      sample_id = stringr::str_remove(dataset_sample_id, ".*__")
    ) %>%
    dplyr::group_by(dataset_id) %>%
    tidyr::nest()

  return(results)
}

