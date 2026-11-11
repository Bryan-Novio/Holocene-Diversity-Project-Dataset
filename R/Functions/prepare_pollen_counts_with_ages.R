#' @title Join pollen counts with sample ages
#' @description
#' Returns long-format pollen counts together with the age of each sample, by
#' joining the outputs of [prepare_pollen_counts()] and [prepare_pollen_ages()].
#' @param data_compilation
#' Data frame with one row per dataset and the columns `dataset_id`,
#' `raw_counts`, and `levels`. `raw_counts` is a list-column of data frames
#' with `sample_id` and one numeric column per taxon; `levels` is a
#' list-column of data frames with `sample_id` and `age`.
#' @return
#' A tibble with the columns `dataset_id`, `sample_id`, `taxa`,
#' `pollen_counts`, and `age`.
#' @details
#' The two tables are combined with an inner join by `dataset_id` and
#' `sample_id`, so samples missing from `levels` (or from `raw_counts`) are
#' dropped. A sample that is listed in `levels` with `age = NA` is kept,
#' with `NA` as its age; filter such rows before binning or modelling.
#' Counts that are `NA` are dropped before the join, see
#' [prepare_pollen_counts()]. A sample id that occurs more than once in
#' `levels` of one dataset duplicates its count rows, and [dplyr::inner_join()]
#' then warns about a many-to-many relationship if the sample has more than
#' one taxon. A compilation with zero rows returns a zero-row tibble.
#' @seealso [prepare_pollen_counts()], [prepare_pollen_ages()]
#' @examples
#' \dontrun{
#' data_counts_ages <-
#'   prepare_pollen_counts_with_ages(data_compilation = data_compilation)
#' }
prepare_pollen_counts_with_ages <- function(data_compilation) {

  assertthat::assert_that(
    is.data.frame(data_compilation),
    msg = "data_compilation has to be data.frame"
  )

  assertthat::assert_that(
    all(
      c("dataset_id", "raw_counts", "levels") %in% colnames(data_compilation)
    ),
    msg = "`data_compilation` must contain: dataset_id, raw_counts, levels."
  )
  
  data_pollen <- prepare_pollen_counts(data_compilation)
  
  data_ages <- prepare_pollen_ages(data_compilation)
  
  inner_join(data_pollen, data_ages,
             by = c("dataset_id", 'sample_id'))
  
}