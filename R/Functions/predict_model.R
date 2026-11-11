#' @title Predict from a GAM, optionally excluding a factor term
#' @description
#' Computes model predictions for `newdata` with
#' [marginaleffects::predictions()] and returns them as a tibble. Optionally,
#' all smooth terms of one factor variable (e.g. a random effect of
#' `dataset_id`) are excluded from the prediction, giving population-level
#' estimates.
#' @param model
#' A fitted model of class `gam` (from [mgcv::gam()] or [mgcv::bam()]).
#' @param newdata
#' Data frame with the predictor values to predict for. It must contain all
#' predictors of `model`, except the variable named in `exclude_terms`,
#' which is filled in automatically.
#' @param type
#' Single character string. Prediction scale passed to
#' [marginaleffects::predictions()], such as `"response"` (default) or
#' `"link"`.
#' @param exclude_terms
#' `NULL` (default) or a single character string naming one factor variable
#' of `model`. Every smooth term whose label contains this name is excluded
#' from the prediction (via [gratia::smooths()]), and the variable is set to
#' its first level in `newdata`. Only one term is supported.
#' @param ...
#' Further arguments passed to [marginaleffects::predictions()], such as
#' `vcov` or `conf_level`.
#' @return
#' A tibble with one row per row of `newdata`. It holds the columns of
#' `newdata` with their original names, plus the prediction columns from
#' [marginaleffects::predictions()] with names cleaned to snake case by
#' [janitor::make_clean_names()] (e.g. `estimate`, `std_error`,
#' `conf_low`, `conf_high`).
#' @details
#' When `exclude_terms` is given, the value written into the excluded
#' variable does not affect the estimate, because all smooths of that
#' variable are dropped. It is only needed because
#' [marginaleffects::predictions()] requires every model variable to be
#' present. Matching of smooth labels uses a regular expression, so a term
#' name that is a substring of another variable name also excludes the
#' smooths of that variable.
#' @seealso [fit_regression_model()], [extract_factor_levels()]
#' @examples
#' \dontrun{
#' data_predicted <-
#'   predict_model(
#'     model = mod_richness,
#'     newdata = data_new_ages,
#'     type = "response",
#'     exclude_terms = "dataset_id"
#'   )
#' }
predict_model <- function(
  model, newdata,
  type = "response",
  exclude_terms = NULL,
  ...
) {
  assertthat::assert_that(
    inherits(model, "gam"),
    msg = "`model` must be a 'gam' object."
  )

  assertthat::assert_that(
    is.data.frame(newdata),
    msg = "`newdata` must be a data frame."
  )

  assertthat::assert_that(
    length(type) == 1,
    is.character(type),
    msg = "`type` must be a single character string."
  )

  assertthat::assert_that(
    is.null(exclude_terms) ||
      (length(exclude_terms) >= 1 &&
        is.character(exclude_terms)),
    msg = "`exclude_terms` must be NULL or a character vector."
  )

  data_to_predict <-
    newdata

  terms_to_exclude <- NULL
  if (
    is.null(exclude_terms) == FALSE
  ) {
    terms_to_exclude <-
      gratia::smooths(model) |>
      stringr::str_subset(exclude_terms)

    vec_factor_levels <-
      extract_factor_levels(
        model,
        sel_term = exclude_terms
      )

    data_to_predict <-
      newdata |>
      dplyr::mutate(
        # This the data have a factor with one level.
        # The selected level does not matter as it will be excluded but
        # it is required to be present in the data.
        !!exclude_terms := vec_factor_levels[1]
      )
  }

  data_predicted <-
    marginaleffects::predictions(
      model,
      newdata = data_to_predict,
      type = type,
      exclude = terms_to_exclude,
      ...
    ) |>
    as.data.frame() |>
    tibble::as_tibble()

  res <- data_predicted

  # those data columns which are NOT present in the newdata
  # should be cleaned via janitor::make_clean_names()

  cols_newdata <- colnames(newdata)
  cols_res <- colnames(data_predicted)
  cols_to_clean <-
    cols_res[!(cols_res %in% cols_newdata)]

  if (
    length(cols_to_clean) > 0
  ) {
    res <-
      data_predicted |>
      dplyr::rename_with(
        .cols = dplyr::all_of(cols_to_clean),
        .fn = janitor::make_clean_names
      )
  }

  return(res)
}
