#' @title Extract sample ages from a compilation
#' @description
#' Unnests the `levels` list-column of a pollen data compilation and returns
#' the age of every sample of every dataset.
#' @param data_compilation
#' Data frame with one row per dataset and the columns `dataset_id` and
#' `levels`. `levels` is a list-column of data frames, each with the columns
#' `sample_id` and `age` (numeric) and possibly further columns such as
#' `depth`.
#' @return
#' A tibble with the columns `dataset_id`, `sample_id`, and `age`, one row
#' per sample.
#' @details
#' Only `dataset_id`, `sample_id`, and `age` are kept; all other columns of
#' `levels` are dropped. Every `levels` table must contain `sample_id` and
#' `age`; this is not checked explicitly, so a missing column fails inside
#' [dplyr::select()]. Datasets whose `levels` table has zero rows contribute
#' no rows. Known gap: a compilation with zero rows fails inside
#' [dplyr::select()] (no `sample_id` column after unnesting); a zero-row
#' tibble would be the intended result.
#' @seealso [prepare_pollen_counts()], [prepare_pollen_counts_with_ages()]
#' @examples
#' \dontrun{
#' data_ages <-
#'   prepare_pollen_ages(data_compilation = data_compilation)
#' }
prepare_pollen_ages <- function(data_compilation){
  assertthat::assert_that(
    is.data.frame(data_compilation),
    msg = "data_compilation has to be a data.frame"
  )

  assertthat::assert_that(
    all(c("dataset_id", "levels") %in% colnames(data_compilation)),
    msg = "`data_compilation` must contain columns 'dataset_id' and 'levels'."
  )
  
  data_compilation %>% 
    select(dataset_id, levels) %>% 
    unnest(levels) %>% 
    select(dataset_id,sample_id, age)
}