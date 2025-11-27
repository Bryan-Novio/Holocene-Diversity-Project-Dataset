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

  terms_to_exclude <- NULL
  if (
    is.null(exclude_terms) == FALSE
  ) {
    terms_to_exclude <-
      gratia::smooths(model) %>%
      stringr::str_subset(exclude_terms)
  }

  res <-
    marginaleffects::predictions(
      model,
      newdata = newdata,
      type = type,
      exclude = terms_to_exclude,
      ...
    ) %>%
    as.data.frame() %>%
    tibble::as_tibble()

  return(res)
}
