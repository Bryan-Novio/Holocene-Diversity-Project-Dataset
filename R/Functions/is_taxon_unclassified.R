#' @title Flag a failed taxon classification
#' @description
#' Identifies whether a call to a taxonomic classification failed to return
#' a classification. It is used on the output of
#' `purrr::safely(taxospace::get_classification)()` to find taxa that could
#' not be classified.
#' @param x
#' A list with an element named `result`, as returned by
#' `purrr::safely()`. The `result` element is either a data frame (the
#' output of `taxospace::get_classification()`) or `NULL` (when the call
#' failed). Other elements, such as `error`, are ignored.
#' @return
#' A single logical value. `TRUE` when `result` has no column named
#' `classification` (including when `result` is `NULL` or has no columns),
#' otherwise `FALSE`.
#' @details
#' Only the presence of the `classification` column is checked, not its
#' content, so a `classification` column holding only `NA` values counts as
#' classified. The function aborts when `x` is not a list, has no `result`
#' element, or when `result` is neither `NULL` nor a data frame.
#' @seealso [harmonise_taxa()]
#' @examples
#' \dontrun{
#' list_classification <-
#'   purrr::safely(taxospace::get_classification)(
#'     taxon_name = "Abies alba"
#'   )
#'
#' flag_failed <-
#'   is_taxon_unclassified(x = list_classification)
#' }
is_taxon_unclassified <- function(x) {
  assertthat::assert_that(
    is.list(x),
    msg = "`x` must be a list."
  )

  assertthat::assert_that(
    "result" %in% names(x),
    msg = "`x` must contain an element named 'result'."
  )

  assertthat::assert_that(
    is.null(x[["result"]]) || is.data.frame(x[["result"]]),
    msg = "The 'result' element of `x` must be NULL or a data frame."
  )

  name_cols <- colnames(x$result)

  is_classification_present <- "classification" %in% name_cols

  res <- !is_classification_present

  return(res)
}
