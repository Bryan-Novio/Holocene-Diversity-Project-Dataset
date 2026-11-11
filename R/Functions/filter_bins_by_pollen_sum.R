#' @title Keep only bins with a minimum total pollen count
#' @description
#' Filters binned pollen data to the bins of each dataset (core) whose total
#' number of pollen grains reaches a threshold, and returns all rows of those
#' bins.
#' @param data_input
#' Data frame of binned pollen data (e.g. from [bin_pollen_counts()]) with
#' the columns `dataset_id`, `BIN`, and `summed_pollen_count` (numeric).
#' Other columns are kept.
#' @param n_grains
#' Single non-missing number. Minimum total pollen count a bin of a dataset
#' must have to be kept. A total exactly equal to `n_grains` is kept.
#' @return
#' A tibble or data frame with the columns of `data_input`, restricted to the
#' rows whose `dataset_id` and `BIN` combination passed the threshold. Row
#' order follows `data_input`.
#' @details
#' The total is the sum of `summed_pollen_count` over all taxa within each
#' `dataset_id` and `BIN`. Each dataset is judged on its own grains, so the
#' same `BIN` can be kept in one dataset and dropped in another. `NA` values
#' in `summed_pollen_count` make the total of their bin `NA`, and that bin is
#' dropped.
#' @seealso [bin_pollen_counts()], [filter_datasets_by_n_bins()]
#' @examples
#' \dontrun{
#' data_filtered <-
#'   filter_bins_by_pollen_sum(
#'     data_input = data_binned,
#'     n_grains = 400
#'   )
#' }
filter_bins_by_pollen_sum <-
  function(data_input, n_grains) {
    
    assertthat::assert_that(
      is.data.frame(data_input),
      msg = "data_input has to be a data.frame"
    )
    
    assertthat::assert_that(
      all(
        c("dataset_id", "BIN", "summed_pollen_count") %in%
          colnames(data_input)
      ),
      msg = "data_input must contain 'dataset_id', 'BIN', 'summed_pollen_count'"
    )
    
    assertthat::assert_that(
      is.numeric(n_grains) && length(n_grains) == 1 && !is.na(n_grains),
      msg = "n_grains has to be a single numeric value"
    )
  
    # Sum the grains of all taxa within each bin of each dataset
    data_valid_BINS <- data_input %>%
      dplyr::group_by(dataset_id, BIN) %>%
      dplyr::summarise(
        total_pollen = sum(summed_pollen_count),
        .groups = "drop"
      ) %>%
      dplyr::filter(
        total_pollen >= n_grains
      ) %>%
      dplyr::select(dataset_id, BIN)

    res <- inner_join(data_input, data_valid_BINS, by = c("dataset_id", "BIN"))
    

    return(res)
  }