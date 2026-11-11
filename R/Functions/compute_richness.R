#' @title Estimate taxa richness per dataset and age
#' @description
#' Counts the number of taxa present (at least one pollen grain) in each
#' sample, defined by `dataset_id` and `age`.
#' @param data_for_richness_estimation
#' Data frame in long format with the columns `dataset_id`, `age`, and
#' `pollen_grains` (numeric). Each row is one taxon in one sample.
#' @return
#' An ungrouped tibble with the columns `dataset_id`, `age`, and `richness`
#' (numeric), one row per `dataset_id` and `age` combination.
#' @details
#' A row counts as present if `pollen_grains >= 1`. Rows with `NA` or smaller
#' values are not counted, so a group without any present row has richness
#' 0. Each row is counted once, so duplicated taxa within a sample are
#' counted multiple times; the function does not check for duplicates.
#' @seealso [prepare_data_for_richness_estimation()]
#' @examples
#' \dontrun{
#' data_richness <-
#'   compute_richness(
#'     data_for_richness_estimation = data_prepared
#'   )
#' }
compute_richness <- function(data_for_richness_estimation){
  assertthat::assert_that(
  is.data.frame(data_for_richness_estimation),
  msg = "data_for_richness_estimation has to be a data.frame"
  )

  assertthat::assert_that(
    all(
      c("dataset_id", "age", "pollen_grains") %in%
        colnames(data_for_richness_estimation)
    ),
    msg = "Input must contain columns 'dataset_id', 'age', 'pollen_grains'."
  )

  assertthat::assert_that(
    is.numeric(data_for_richness_estimation[["pollen_grains"]]),
    msg = "'pollen_grains' in data_for_richness_estimation must be numeric."
  )
  
  res <-
    data_for_richness_estimation %>%
    mutate(present = ifelse(pollen_grains >= 1, 1, 0)) %>%
    group_by(dataset_id,age) %>%
    summarize(richness = sum(present, na.rm = TRUE), .groups = "drop")

  return(res)
}

