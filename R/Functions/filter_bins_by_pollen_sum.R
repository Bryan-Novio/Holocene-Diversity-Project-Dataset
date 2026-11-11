#' @title Keep only bins with a minimum total pollen count
#' @description
#' Filters binned pollen data to the age bins whose total number of pollen
#' grains reaches a threshold, and returns all rows of those bins.
#' @param data_input
#' Data frame of binned pollen data (e.g. from [bin_pollen_counts()]) with
#' the columns `BIN` and `summed_pollen_count` (numeric). Other columns are
#' kept.
#' @param n_grains
#' Single non-missing number. Minimum total pollen count a bin must have to be
#' kept. A bin with a total exactly equal to `n_grains` is kept.
#' @return
#' A tibble or data frame with the columns of `data_input`, restricted to the
#' rows whose `BIN` passed the threshold. Row order follows `data_input`.
#' @details
#' Current behaviour. The total is the sum of `summed_pollen_count` per `BIN`
#' across ALL rows of `data_input`, i.e. across all datasets and taxa. A bin
#' is therefore kept when the pooled total is at least `n_grains`, even if no
#' single dataset has that many grains in the bin.
#'
#' Ambiguity. The calling script
#' (`p1_study_2/01_bin_data.R`) describes the step as filtering out bins with
#' fewer than 400 pollen grains and then counts bins per core, which suggests
#' that the total should be taken per `dataset_id` and `BIN`. This is not
#' implemented, and the behaviour is deliberately unchanged. `NA` values in
#' `summed_pollen_count` make the total of their bin `NA`, and that bin is
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
      all(c("BIN", "summed_pollen_count") %in% colnames(data_input)),
      msg = "data_input must contain 'BIN' and 'summed_pollen_count'"
    )
    
    assertthat::assert_that(
      is.numeric(n_grains) && length(n_grains) == 1 && !is.na(n_grains),
      msg = "n_grains has to be a single numeric value"
    )
  
    data_valid_BINS <- data_input %>%
      dplyr::group_by(BIN) %>%
      dplyr::summarise(
        total_pollen = sum(summed_pollen_count)
      ) %>%
      dplyr::filter(
        total_pollen >= n_grains
      ) %>% 
      dplyr::select(BIN)
    
    res <- inner_join(data_input, data_valid_BINS, by ="BIN")
    

    return(res)
  }