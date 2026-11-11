#' @title Rarefy every sample of a count table to a fixed number of grains
#' @description
#' Randomly sub-samples the pollen counts of each row (sample) to exactly
#' `n_grains` grains without replacement, using [vegan::rrarefy()].
#' @param data_source
#' Data frame in wide format with one row per sample. It must contain the
#' columns `dataset_id` and `age`, and at least two further columns of
#' non-negative whole-number pollen counts, one per taxon. `NA` counts are
#' treated as 0. The combination of `dataset_id` and `age` must be unique
#' across rows.
#' @param n_grains
#' Single positive whole number. Number of grains to draw from each sample.
#' @return
#' A tibble with one row per row of `data_source` (same order), the column
#' `dataset_id_age` (`"<dataset_id>_<age>"`), and one numeric column per
#' taxon holding the rarefied counts. The columns `dataset_id` and `age` are
#' not returned.
#' @details
#' Rarefaction is random; set a seed for reproducible results. Rarefied counts
#' never exceed the original counts, and every row with at least `n_grains`
#' grains sums to exactly `n_grains`. A sample with fewer than `n_grains`
#' grains cannot be rarefied: [vegan::rrarefy()] then warns and the row is
#' returned unchanged, so its total stays below `n_grains`. Callers must drop
#' such rows if a common depth is required. Non-integer counts make
#' [vegan::rrarefy()] fail. [vegan::rrarefy()] also warns if the smallest
#' positive count of the whole table is above 1, because the input then does
#' not look like raw counts.
#' @seealso [save_rarefied_iterations()], [prepare_pollen_counts_with_ages()]
#' @examples
#' \dontrun{
#' set.seed(900723)
#' data_rarefied <-
#'   rarefy_all_samples(
#'     data_source = data_pollen_wide,
#'     n_grains = 300
#'   )
#' }
rarefy_all_samples <- function(data_source, n_grains) {
  # we expect that the data looks like
  # dataset_id, samples, ...
  
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_source has to be data.frame"
  )
  
  assertthat::assert_that(
    all(c("dataset_id", "age") %in% colnames(data_source)),
    msg = "data must contain columns 'dataset_id', 'age' "
  )
  
  assertthat::assert_that(
    nrow(data_source) > 0,
    msg = "data_source must have at least one row"
  )
  
  assertthat::assert_that(
    ncol(data_source) > 3,
    msg = "data_source must contain at least two taxon columns"
  )
  
  assertthat::assert_that(
    anyDuplicated(
      as.data.frame(data_source)[, c("dataset_id", "age")]
    ) == 0,
    msg = "dataset_id and age must be unique across rows"
  )
  
  assertthat::assert_that(
    is.numeric(n_grains),
    length(n_grains) == 1,
    is.finite(n_grains),
    n_grains > 0,
    n_grains == round(n_grains),
    msg = "n_grains must be a single positive whole number"
  )
  
  data_prepared <-
    data_source %>%
    dplyr::mutate(
      dataset_id_age= paste0(dataset_id,"_", age)
    ) %>%
    dplyr::select(-c(dataset_id,age)) %>% 
    dplyr::mutate(
      dplyr::across(-dataset_id_age, ~ tidyr::replace_na(.,0))
    ) %>% 
    tibble::column_to_rownames("dataset_id_age")
  
  
  results <-
    vegan::rrarefy(data_prepared, sample = n_grains) %>%
    as_tibble(rownames = "dataset_id_age") # convert matrix to data frame
  
  return(results)
}
