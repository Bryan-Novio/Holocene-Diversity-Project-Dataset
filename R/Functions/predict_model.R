predict_model <- function(
  model, newdata,
  type = "response",
  exclude_terms = NULL,
  ...
) {
  require(dplyr)
  require(assertthat)
  require(marginaleffects)
  require(gratia)
  require(janitor)
  require(stringr)

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
