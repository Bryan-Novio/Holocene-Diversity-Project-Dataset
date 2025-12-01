extract_factor_levels <- function(model, sel_term) {
  require(dplyr)
  require(stringr)
  require(assertthat)
  require(insight)

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
