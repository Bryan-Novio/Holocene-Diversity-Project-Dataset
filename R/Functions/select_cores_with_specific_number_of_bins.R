select_cores_with_specific_number_of_bins <- function(data_input, n_bins) {
  
  assertthat::assert_that(
    is.data.frame(data_input),
    msg = "data_input has to be data.frame"
  )
  
  assertthat::assert_that(
    is.numeric(n_bins) && length(n_bins) == 1 && !is.na(n_bins),
    msg = "n_bins must be a single numeric, non-NA value"
  )
  
  data_n_bins <- data_input %>%
    dplyr::distinct(dataset_id, BIN) %>%
    dplyr::group_by(dataset_id) %>%
    dplyr::summarise(
      n = dplyr::n()
    ) %>%
    dplyr:::ungroup() %>%
    dplyr::filter(
      n >= n_bins
    ) %>% 
    select(dataset_id)

  res <-
    data_input %>%
    dplyr::inner_join(
      data_n_bins,
      by = dplyr::join_by(dataset_id)
    )

  return(res)
}
