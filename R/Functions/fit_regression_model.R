fit_regression_model <- function(
  data_source,
  y_var, time_var, group_var,
  random = c("intercept", "slope"),
  sel_k,
  error_family = stats::gaussian(),
  ...
) {
  require(mgcv)
  require(stringr)
  require(assertthat)

  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_source must be a data.frame"
  )

  assertthat::assert_that(
    length(y_var) == 1,
    is.na(y_var) == FALSE,
    is.character(y_var),
    msg = "y_var must be a single character string"
  )

  assertthat::assert_that(
    length(time_var) == 1,
    is.na(time_var) == FALSE,
    is.character(time_var),
    msg = "time_var must be a single character string"
  )

  assertthat::assert_that(
    length(group_var) == 1,
    is.na(group_var) == FALSE,
    is.character(group_var),
    msg = "group_var must be a single character string"
  )

  assertthat::assert_that(
    all(c(y_var, time_var, group_var) %in% names(data_source)),
    msg = stringr::str_glue(
      "data_source must be a containing the variables: {y_var}, {time_var}, {group_var}"
    )
  )

  random <-
    match.arg(random,
      choices = c("intercept", "slope")
    )

  assertthat::assert_that(
    length(sel_k) == 1,
    is.numeric(sel_k),
    is.na(sel_k) == FALSE,
    sel_k > 0,
    msg = "sel_k must be a single positive numeric value"
  )

  assertthat::assert_that(
    inherits(error_family, "family"),
    msg = "error_family must be a valid family object"
  )

  sel_formula <-
    switch(random,
      "intercept" =
        stringr::str_glue(
          "{y_var} ~ s({time_var}, k = {sel_k}, bs = 'cr') + s({group_var}, bs = 're')"
        ),
      "slope" =
        stringr::str_glue(
          "{y_var} ~ s({time_var}, k = {sel_k}, bs = 'cr') + s({time_var}, {group_var}, k = {sel_k}, bs = 'fs', xt = list(bs = 'cr'))"
        )
    ) |>
    as.formula()

  mod <-
    mgcv::bam(
      formula = sel_formula,
      data = data_source,
      method = "fREML",
      family = error_family,
      ...
    )

  return(mod)
}
