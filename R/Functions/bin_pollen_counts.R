#' @title Bin pollen counts into fixed-width age bins
#' @description
#' Assigns each sample to an age bin of fixed width and sums the pollen
#' counts of each taxon within each combination of a binning variable (e.g.
#' `dataset_id`) and age bin.
#' @param data_source
#' Data frame in long format with the columns `age` (numeric, no `NA`),
#' `pollen_counts` (numeric, no `NA`), `taxa` (taxon identifier), and the
#' column named by `binning_var`. It must have at least one row.
#' @param binning_var
#' Name of the column to bin within (e.g. `dataset_id`), given as a bare
#' column name or as a string. It must be a column of `data_source`.
#' @param bin_size
#' Single positive number. Width of one bin, in the units of `age`.
#' @return
#' An ungrouped tibble with one row per `binning_var`, `taxa`, and `BIN`, and
#' the columns `binning_var` (under its original name), `taxa`, `BIN`
#' (integer bin index), and `summed_pollen_count` (sum of `pollen_counts`).
#' @details
#' Bins are half-open intervals `[min_age + (i - 1) * bin_size, min_age + i *
#' bin_size)`, starting at the smallest `age` of the WHOLE input (not per
#' `binning_var`). `BIN` is the chronological bin index
#' `i = floor((age - min_age) / bin_size) + 1`, so the youngest bin is 1. A
#' bin without samples leaves a gap in `BIN` (e.g. 1, 2, 4); the later bins
#' keep their index.
#' @seealso [filter_bins_by_pollen_sum()]
#' @examples
#' \dontrun{
#' data_binned <-
#'   bin_pollen_counts(
#'     data_source = data_pollen_with_ages,
#'     binning_var = dataset_id,
#'     bin_size = 500
#'   )
#' }
bin_pollen_counts <- function(data_source, binning_var, bin_size) {
  # Check that data_source is a data frame
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_source has to be a data.frame"
  )

  # Check that the required columns exist
  required_cols <- c("age", "pollen_counts", "taxa")
  missing_cols <- setdiff(required_cols, colnames(data_source))
  assertthat::assert_that(
    length(missing_cols) == 0,
    msg = paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
  )

  # Check that there is at least one row to bin
  assertthat::assert_that(
    nrow(data_source) > 0,
    msg = "data_source must have at least one row"
  )

  # Check that ages and counts are numeric
  assertthat::assert_that(
    is.numeric(data_source[["age"]]),
    is.numeric(data_source[["pollen_counts"]]),
    msg = "age and pollen_counts must be numeric"
  )

  # Capture the bare column name of 'binning_var' to check that it exists
  bin_col <- rlang::ensym(binning_var)
  assertthat::assert_that(
    rlang::as_string(bin_col) %in% colnames(data_source),
    msg = "binning_var column is missing in data_source"
  )

  # Check that bin_size is a single positive number
  assertthat::assert_that(
    is.numeric(bin_size) && length(bin_size) == 1 && !is.na(bin_size) &&
      bin_size > 0,
    msg = "bin_size must be a single positive numeric value"
  )

  # Reject missing ages and counts, which cannot be binned or summed
  assertthat::assert_that(
    !any(is.na(data_source$age)) && !any(is.na(data_source$pollen_counts)),
    msg = "age and pollen_counts cannot contain NA values"
  )

  # Compute the bin index directly from age, so that BIN is chronological
  #   and an empty bin leaves a gap instead of shifting the later bins
  data_binned <- data_source %>%
    mutate(
      BIN = as.integer(floor((age - min(age)) / bin_size) + 1)
    ) %>%
    # Group by the captured symbol, which also resolves a quoted column name
    group_by(!!bin_col, taxa, BIN) %>%
    summarise(summed_pollen_count = sum(pollen_counts), .groups = "drop")

  return(data_binned)
}
