#' @title Keep only datasets with a minimum number of bins
#' @description
#' Filters binned pollen data to the datasets (cores) that have samples in at
#' least a given number of distinct age bins, and returns all rows of those
#' datasets.
#' @param data_input
#' Data frame of binned pollen data (e.g. from [bin_pollen_counts()]) with
#' the columns `dataset_id` and `BIN`. Other columns are kept.
#' @param n_bins
#' Single non-missing number. Minimum number of distinct `BIN` values a
#' dataset must have to be kept. A dataset with exactly `n_bins` is kept.
#' @return
#' A tibble or data frame with the columns of `data_input`, restricted to the
#' rows of the datasets that passed the threshold. Row order follows
#' `data_input`.
#' @details
#' Distinct bins are counted per `dataset_id`, regardless of taxa or the
#' number of rows. Bins are not required to be consecutive. `NA` is counted as
#' a bin value of its own.
#' @seealso [bin_pollen_counts()], [filter_bins_by_pollen_sum()]
#' @examples
#' \dontrun{
#' data_cores <-
#'   filter_datasets_by_n_bins(
#'     data_input = data_binned,
#'     n_bins = 5
#'   )
#' }
filter_datasets_by_n_bins <- function(data_input, n_bins) {
  
  assertthat::assert_that(
    is.data.frame(data_input),
    msg = "data_input has to be data.frame"
  )
  
  assertthat::assert_that(
    is.numeric(n_bins) && length(n_bins) == 1 && !is.na(n_bins),
    msg = "n_bins must be a single numeric, non-NA value"
  )
  
  assertthat::assert_that(
    all(c("dataset_id", "BIN") %in% colnames(data_input)),
    msg = "data_input must contain 'dataset_id' and 'BIN'"
  )
  
  data_n_bins <- data_input %>%
    dplyr::distinct(dataset_id, BIN) %>%
    dplyr::group_by(dataset_id) %>%
    dplyr::summarise(
      n = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
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
