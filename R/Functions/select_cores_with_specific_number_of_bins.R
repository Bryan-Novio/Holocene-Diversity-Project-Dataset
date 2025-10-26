select_cores_with_specific_number_of_bins <- function(data_input, n_bins) {
  
  assertthat::assert_that(
    is.data.frame(data_input),
    msg = "data_input has to be data.frame"
  )
  
  data_n_bins <- data_input %>%
    dplyr::distinct(dataset_id, BIN, sample_id) %>%
    dplyr::group_by(dataset_id, BIN) %>%
    dplyr::summarise(
      n = dplyr::n()
    ) %>%
    dplyr:::ungroup() %>%
    dplyr::filter(
      n >= n_bins
    )

  res <-
    data_input %>%
    dplyr::inner_join(
      data_n_bins,
      by = dplyr::join_by(dataset_id, BIN)
    )

  return(res)
}
