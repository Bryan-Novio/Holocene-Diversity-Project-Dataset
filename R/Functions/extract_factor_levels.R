#' @title Extract the distinct values of a model predictor
#' @description
#' Returns the distinct values of the one predictor of a fitted GAM whose
#' name starts with `sel_term`. It is used to read the levels of a factor
#' variable (such as `dataset_id`) directly from a fitted model.
#' @param model
#' A fitted model of class `gam` or `bam` (from [mgcv::gam()] or
#' [mgcv::bam()]).
#' @param sel_term
#' Single character string. The start of the name of the predictor to
#' extract. Exactly one predictor of `model` must start with this string.
#' @return
#' A vector with the distinct values of the matching predictor, in order of
#' appearance in the model data. A factor stays a factor, with all of its
#' original levels.
#' @details
#' Predictors are read with [insight::get_predictors()]. The match is by
#' prefix and ignores case (`dplyr::starts_with()`), so `sel_term = "group"`
#' also matches a predictor called `group_type` or `Group`. The function
#' aborts when no predictor or more than one predictor matches. The distinct
#' values are those found in the data used to fit the model, which are not
#' necessarily all levels of the factor.
#' @seealso [predict_model()], [fit_regression_model()]
#' @examples
#' \dontrun{
#' vec_dataset_ids <-
#'   extract_factor_levels(
#'     model = mod_richness,
#'     sel_term = "dataset_id"
#'   )
#' }
extract_factor_levels <- function(model, sel_term) {
  assertthat::assert_that(
    inherits(
      model,
      c("gam", "bam")
    ),
    msg = "`model` must be a GAM/BAM object."
  )

  assertthat::assert_that(
    length(sel_term) == 1,
    is.character(sel_term),
    is.na(sel_term) == FALSE,
    msg = "`sel_term` must be a single character string."
  )

  data_predictors <-
    model |>
    insight::get_predictors() |>
    dplyr::select(
      dplyr::starts_with(sel_term)
    )

  assertthat::assert_that(
    ncol(data_predictors) > 0,
    msg = paste0(
      "No predictor variables found for term '",
      sel_term,
      "'. Please ensure 'sel_term' matches a single factor variable in the model."
    )
  )

  assertthat::assert_that(
    ncol(data_predictors) < 2,
    msg = paste0(
      "Multiple predictor variables found for term '",
      sel_term,
      "'. Please ensure 'sel_term' matches a single factor variable in the model."
    )
  )

  vec_levels <-
    data_predictors %>%
    dplyr::distinct() |>
    dplyr::pull()

  assertthat::assert_that(
    length(vec_levels) >= 1,
    msg = paste0(
      "No factor levels found for term '",
      sel_term,
      "'."
    )
  )

  return(vec_levels)
}
