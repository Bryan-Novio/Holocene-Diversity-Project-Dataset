#' @title Extract long-format pollen counts from a compilation
#' @description
#' Unnests the `raw_counts` list-column of a pollen data compilation and
#' returns the counts of all taxa in long format, dropping missing counts.
#' @param data_compilation
#' Data frame with one row per dataset and the columns `dataset_id` and
#' `raw_counts`. `raw_counts` is a list-column of data frames, each with a
#' column `sample_id` and one numeric column per taxon (the column name is
#' the taxon name).
#' @return
#' A tibble in long format with the columns `dataset_id`, `sample_id`,
#' `taxa` (taxon name, character), and `pollen_counts` (numeric). It has one
#' row per dataset, sample, and taxon with a non-missing count.
#' @details
#' Rows with `NA` counts are dropped, whereas zero counts are kept. Every
#' `raw_counts` table must contain a column `sample_id`; this is not checked
#' explicitly, so a missing column fails inside [tidyr::pivot_longer()]. All
#' columns other than `dataset_id` and `sample_id` are treated as taxa.
#' A compilation with zero rows, or whose count tables have no taxon
#' columns, returns a zero-row tibble with the output columns.
#' @seealso [prepare_pollen_ages()], [prepare_pollen_counts_with_ages()]
#' @examples
#' \dontrun{
#' data_counts <-
#'   prepare_pollen_counts(data_compilation = data_compilation)
#' }
prepare_pollen_counts <- function(data_compilation) {
  
  
  assertthat::assert_that(
    is.data.frame(data_compilation),
    msg = "`data_compilation` must be a data frame or tibble."
  )
  
  assertthat::assert_that(
    all(c("dataset_id", "raw_counts") %in% colnames(data_compilation)),
    msg = "`data_compilation` must contain columns 'dataset_id', 'raw_counts'."
  )

  # The empty result, with the output columns
  data_empty <-
    tibble::tibble(
      dataset_id = data_compilation[["dataset_id"]][0],
      sample_id = character(),
      taxa = character(),
      pollen_counts = numeric()
    )

  # A compilation without datasets has no nested tables to unnest
  if (nrow(data_compilation) == 0) {
    return(data_empty)
  }

  data_unnested <-
    data_compilation %>%
    select(dataset_id, raw_counts) %>%
    unnest(raw_counts)

  # Count tables without any taxon column leave nothing to pivot
  vec_taxa_cols <-
    setdiff(colnames(data_unnested), c("dataset_id", "sample_id"))

  if (length(vec_taxa_cols) == 0) {
    return(data_empty)
  }

  res <-
    data_unnested %>%
    pivot_longer(
      cols = !c(dataset_id,sample_id),
      names_to = "taxa", values_to = "pollen_counts",
      values_drop_na = TRUE)
  
  return(res)
}