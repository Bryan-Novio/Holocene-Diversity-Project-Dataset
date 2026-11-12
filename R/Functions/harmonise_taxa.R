#' @title Harmonise taxon names and sum their pollen counts
#' @description
#' Replaces each original taxon name with its harmonised name from a
#' harmonisation table and sums pollen counts of taxa that share the same
#' harmonised name within each dataset and age.
#' @param data_to_harmonize
#' Data frame in long format with columns `dataset_id`, `age`, `taxon_name`
#' (original name), and `pollen_counts` (numeric).
#' @param harmonisation_table
#' Data frame with a column `taxon_name` (original names) and a column named
#' by `level` holding the harmonised names. Every `taxon_name` present in
#' `data_to_harmonize` must be present in this table, and each original name
#' must map to a single harmonised name.
#' @param level
#' Single character string. Name of the column of `harmonisation_table`
#' holding the harmonised names (e.g. `"genus"`).
#' @return
#' A tibble with one row per `dataset_id`, `age`, and harmonised taxon and the
#' columns `dataset_id`, `age`, `taxon_name` (harmonised name), and
#' `pollen_counts` (summed counts).
#' @details
#' Exact duplicate rows of the harmonisation table are ignored. Rows are
#' aggregated by `dataset_id` and `age`, so two samples sharing the same age
#' within one dataset are merged. Original taxa mapped to a missing (`NA`)
#' harmonised name are kept and summed together under `taxon_name = NA`;
#' whether such taxa should instead be dropped is not defined.
#' @seealso [prepare_pollen_counts_with_ages()]
#' @examples
#' \dontrun{
#' data_harmonised <-
#'   harmonise_taxa(
#'     data_to_harmonize = data_pollen,
#'     harmonisation_table = data_harmonisation_table,
#'     level = "genus"
#'   )
#' }
harmonise_taxa <- function(data_to_harmonize, harmonisation_table, level) {
  assertthat::assert_that(
    is.data.frame(data_to_harmonize),
    msg = "data_to_harmonize has to be data.frame"
  )

  assertthat::assert_that(
    all(
      c("dataset_id", "age", "taxon_name", "pollen_counts") %in%
        colnames(data_to_harmonize)
    ),
    msg = "data must contain 'dataset_id', 'age', 'taxon_name', 'pollen_counts'"
  )

  assertthat::assert_that(
    is.numeric(data_to_harmonize[["pollen_counts"]]),
    msg = "'pollen_counts' in data_to_harmonize must be numeric"
  )

  assertthat::assert_that(
    is.data.frame(harmonisation_table),
    msg = "harmonisation_table has to be data.frame"
  )

  assertthat::assert_that(
    is.character(level),
    length(level) == 1,
    !is.na(level),
    msg = "level must be a single character string"
  )

  assertthat::assert_that(
    all(c("taxon_name", level) %in% colnames(harmonisation_table)),
    msg = "harmonisation_table must contain 'taxon_name' and the 'level' column"
  )

  taxa_level <- sym(level) # Convert string to symbol for use in dplyr

  harmonisation_table_prep <- harmonisation_table %>%
    dplyr::select(taxon_name, !!taxa_level) %>%
    rlang::set_names(
      nm = c("taxon_name", "taxa_harmonised")
    ) %>%
    dplyr::distinct()

  assertthat::assert_that(
    duplicated(harmonisation_table_prep$taxon_name) %>%
      any() == FALSE,
    msg = "there is duplication of taxon_name in harm table"
  )

  taxa_input <-
    data_to_harmonize %>%
    dplyr::distinct(taxon_name) %>%
    dplyr::mutate(
      present_in_data = TRUE
    )

  taxa_harmonisation_table <-
    harmonisation_table_prep %>%
    dplyr::distinct(taxon_name) %>%
    dplyr::mutate(
      present_in_harm_table = TRUE
    )

  data_taxa_missing <-
    full_join(
      taxa_input,
      taxa_harmonisation_table,
      by = "taxon_name"
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.logical),
        ~ replace_na(.x, FALSE)
      )
    ) %>%
    dplyr::filter(
      present_in_data == TRUE &
        present_in_harm_table == FALSE
    )

  assertthat::assert_that(
    nrow(data_taxa_missing) == 0,
    msg = "harmonisation table must contain taxon_names"
  )

  res <-
    data_to_harmonize %>%
    dplyr::left_join(
      harmonisation_table_prep,
      by = "taxon_name"
    ) %>%
    dplyr::group_by(dataset_id, age, taxa_harmonised) %>%
    dplyr::summarize(
      pollen_sum = sum(pollen_counts),
      .groups = "drop"
    ) %>%
    dplyr::rename(taxon_name = taxa_harmonised) %>%
    dplyr::rename(pollen_counts = pollen_sum)

  return(res)
}
