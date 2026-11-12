#' @title Fit a GAM of a response over time with group structure
#' @description
#' Fits a generalised additive model with [mgcv::bam()] (method `"fREML"`) in
#' which a response changes smoothly over time and groups (e.g. datasets or
#' regions) enter as random effects or group-specific smooths. The model
#' formula is chosen with the `random` argument.
#' @param data_source
#' Data frame with the columns named in `y_var`, `time_var`, and `group_var`.
#' The grouping variable must be a factor. For every `random` option except
#' `"intercept"` and `"slope"` the data must also contain a factor column
#' `dataset_id`.
#' @param y_var
#' Single character string. Name of the response column.
#' @param time_var
#' Single character string. Name of the time (e.g. age) column.
#' @param group_var
#' Single character string. Name of the grouping column (e.g. region or
#' dataset). The column must be a factor.
#' @param random
#' Single character string selecting the model structure. One of the
#' following, with `y`, `t`, `g`, and `k` standing for `y_var`, `time_var`,
#' `group_var`, and `sel_k`:
#' * `"intercept"` (default): `y ~ s(t, k, bs = "cr") +
#'   s(g, bs = "re")`. A common smooth trend plus a random intercept for
#'   each group.
#' * `"slope"`: `y ~ s(t, k, bs = "cr") + s(t, g, k, bs = "fs",
#'   xt = list(bs = "cr"))`. A common smooth trend plus a separate smooth
#'   (random smooth) for each group.
#' * `"slope_rand_id"`: the `"slope"` formula plus a random intercept
#'   `s(dataset_id, bs = "re")`.
#' * `"slope_rand_id_2"`: `y ~ s(t) + s(t, g, k, bs = "fs") +
#'   s(dataset_id, bs = "re")`. As `"slope_rand_id"`, but the common smooth
#'   uses the default basis and dimension (`sel_k` is not applied to it) and
#'   the group smooths use no `xt` setting.
#' * `"intercept_reg"`: `y ~ g + s(t, by = g, bs = "tp", k) +
#'   s(dataset_id, bs = "re")`. A fixed effect of the group plus a separate
#'   smooth for each group, each with its own smoothing parameter, and a
#'   random intercept for `dataset_id`.
#' * `"hgam_GI"`: `y ~ s(t, bs = "tp", k) + s(t, by = g, m = 1, bs = "tp",
#'   k) + s(dataset_id, bs = "re")`. A hierarchical GAM in the style of model
#'   GI (Pedersen et al. 2019): a global smooth plus group-level deviation
#'   smooths, each group with its own smoothing parameter (`m = 1` penalises
#'   the first derivative). Unlike the original GI model, the random
#'   intercept is for `dataset_id`, not for the group.
#' @param sel_k
#' Single positive number. Basis dimension `k` of the time smooths.
#' @param error_family
#' A family object, such as [stats::gaussian()] (default) or
#' [stats::poisson()], passed as `family` to [mgcv::bam()].
#' @param ...
#' Further arguments passed to [mgcv::bam()], such as `weights`, `discrete`,
#' or `nthreads`. The arguments `formula`, `data`, `method`, and `family` are
#' set by the function and cannot be passed here.
#' @return
#' A fitted model of class `bam` (also `gam`, `glm`, and `lm`), estimated with
#' fast REML. Smooth terms can be listed with [gratia::smooths()].
#' @details
#' `random` is resolved with [base::match.arg()], so omitting it selects
#' `"intercept"`.
#'
#' The `"slope"`, `"slope_rand_id"`, and `"slope_rand_id_2"` structures
#' combine `s(t)` with `s(t, g, bs = "fs")`, which makes [mgcv::bam()] warn
#' that the model has repeated 1-d smooths of the same variable. The variable
#' `dataset_id` is hard coded in the formulas of `"slope_rand_id"`,
#' `"slope_rand_id_2"`, `"intercept_reg"`, and `"hgam_GI"`, which is why these
#' options require that column. A grouping variable with a single level makes
#' [mgcv::bam()] abort, because factor contrasts need two or more levels.
#' @seealso [predict_model()], [extract_factor_levels()]
#' @examples
#' \dontrun{
#' mod_richness <-
#'   fit_regression_model(
#'     data_source = data_richness,
#'     y_var = "richness",
#'     time_var = "age",
#'     group_var = "region",
#'     random = "hgam_GI",
#'     sel_k = 10,
#'     error_family = stats::gaussian()
#'   )
#' }
fit_regression_model <- function(
  data_source,
  y_var, time_var, group_var,
  random = c(
    "intercept", "slope", "slope_rand_id", "slope_rand_id_2",
    "intercept_reg", "hgam_GI"
  ),
  sel_k,
  error_family = stats::gaussian(),
  ...
) {
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
      choices = c("intercept", "slope", "slope_rand_id", "slope_rand_id_2", "intercept_reg", "hgam_GI" )
    )

  assertthat::assert_that(
    random %in% c("intercept", "slope") || "dataset_id" %in% names(data_source),
    msg = "data_source must contain 'dataset_id' for this 'random' option"
  )

  # A numeric grouping column would silently turn the random intercept
  #   s(g, bs = "re") into a single slope on the group number
  assertthat::assert_that(
    is.factor(data_source[[group_var]]),
    msg = "the group_var column must be a factor"
  )

  assertthat::assert_that(
    random %in% c("intercept", "slope") ||
      is.factor(data_source[["dataset_id"]]),
    msg = "the dataset_id column must be a factor"
  )

  # These arguments are set below, so passing them in ... would clash
  vec_reserved_args <- c("formula", "data", "method", "family")
  assertthat::assert_that(
    !any(...names() %in% vec_reserved_args),
    msg = "formula, data, method, and family cannot be passed in ..."
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
        ),
      
      "slope_rand_id" =
        stringr::str_glue(
          "{y_var} ~ s({time_var}, k = {sel_k}, bs = 'cr') + s({time_var}, {group_var}, k = {sel_k}, bs = 'fs', xt = list(bs = 'cr')) + s(dataset_id, bs = 're')"
        ),
      
      "slope_rand_id_2" =
        stringr::str_glue(
          "{y_var} ~ s({time_var}) + s({time_var}, {group_var}, k = {sel_k}, bs = 'fs') +  s(dataset_id, bs = 're')"
          ), 
      
      "intercept_reg" = 
        stringr::str_glue(
          "{y_var} ~ {group_var} + s({time_var}, by = {group_var}, bs = 'tp', k = {sel_k}) + s(dataset_id, bs = 're')" 
        ),
      
      "hgam_GI" =  stringr::str_glue(
        "{y_var} ~ s({time_var}, bs = 'tp', k = {sel_k}) + s({time_var},  by = {group_var}, m =1, bs = 'tp', k = {sel_k}) +  s(dataset_id, bs = 're')" 
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
