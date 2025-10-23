select_only_bins_with_specific_pollen_grain_sum <-
  function(data_input, n_grains) {
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
