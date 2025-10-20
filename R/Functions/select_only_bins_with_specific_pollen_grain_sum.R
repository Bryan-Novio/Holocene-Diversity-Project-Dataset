select_only_bins_with_specific_pollen_grain_sum <-
  function(date_input, n_grains) {
    rss <- date_input %>%
      dplyr::group_by(BIN) %>%
      dplyr::summarise(
        total_pollen = sum(summed_pollen_count)
      ) %>%
      dplyr::filter(
        total_pollen >= n_grains
      )

    return(res)
  }
