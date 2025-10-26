select_only_bins_with_specific_pollen_grain_sum <-
  function(data_input, n_grains) {
    assertthat::assert_that(
      is.data.frame(data_input),
      msg = "data_input has to be a data.frame"
    )
  
    res <- data_input %>%
      dplyr::group_by(BIN) %>%
      dplyr::summarise(
        total_pollen = sum(summed_pollen_count)
      ) %>%
      dplyr::filter(
        total_pollen >= n_grains
      )

    return(res)
  }