testthat::test_that("fit_regression_model() validates data_source type and required columns", {
  df_valid <-
    data.frame(
      richness = rnorm(10),
      age = 1:10,
      dataset_id = rep(letters[1:2], length.out = 10) |>
        as.factor()
    )

  testthat::expect_no_error(
    fit_regression_model(
      data_source = df_valid,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 5
    )
  )

  df_extra <-
    data.frame(
      richness = rnorm(10),
      age = 1:10,
      dataset_id = rep(letters[1:2], length.out = 10) |>
        as.factor(),
      extra = rnorm(10)
    )

  testthat::expect_no_error(
    fit_regression_model(
      data_source = df_extra,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 5
    )
  )

  df_missing <-
    data.frame(
      richness = rnorm(10),
      age = 1:10
    )

  testthat::expect_error(
    fit_regression_model(
      data_source = df_missing,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 5
    ),
    "data_source must be a containing the variables"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = as.matrix(df_valid),
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 5
    ),
    "data_source must be a data.frame"
  )
})

testthat::test_that("fit_regression_model() validates y_var, time_var, and group_var", {
  df <-
    data.frame(
      richness = rnorm(10),
      age = 1:10,
      dataset_id = rep(letters[1:2], length.out = 10) |>
        as.factor()
    )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = 1,
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 5
    ),
    "y_var must be a single character string"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = c("richness", "age"),
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 5
    ),
    "y_var must be a single character string"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = NA_character_,
      group_var = "dataset_id",
      sel_k = 5
    ),
    "time_var must be a single character string"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = c("dataset_id", "other"),
      sel_k = 5
    ),
    "group_var must be a single character string"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "not_in_data",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 5
    ),
    "data_source must be a containing the variables"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "not_in_data",
      group_var = "dataset_id",
      sel_k = 5
    ),
    "data_source must be a containing the variables"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "not_in_data",
      sel_k = 5
    ),
    "data_source must be a containing the variables"
  )
})

testthat::test_that("fit_regression_model() validates random argument", {
  df <-
    data.frame(
      richness = rnorm(20),
      age = rep(1:10, 2),
      dataset_id = rep(letters[1:2], each = 10) |>
        as.factor()
    )

  testthat::expect_no_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      random = "intercept",
      sel_k = 5
    )
  )

  testthat::expect_no_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      random = "slope",
      sel_k = 5
    )
  )

  testthat::expect_no_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      random = "both",
      sel_k = 5
    )
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      random = "something_else",
      sel_k = 5
    ),
    "one of"
  )
})

testthat::test_that("fit_regression_model() validates sel_k", {
  df <-
    data.frame(
      richness = rnorm(10),
      age = 1:10,
      dataset_id = rep(letters[1:2], length.out = 10) |>
        as.factor()
    )

  testthat::expect_no_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 3
    )
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = "3"
    ),
    "sel_k must be a single positive numeric value"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = c(3, 4)
    ),
    "sel_k must be a single positive numeric value"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 0
    ),
    "sel_k must be a single positive numeric value"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = -1
    ),
    "sel_k must be a single positive numeric value"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = NA_real_
    ),
    "sel_k must be a single positive numeric value"
  )
})

testthat::test_that("fit_regression_model() validates error_family", {
  df <-
    data.frame(
      richness = rpois(20, lambda = 3),
      age = rep(1:10, 2),
      dataset_id = rep(letters[1:2], each = 10) |>
        as.factor()
    )

  mod <-
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 5,
      error_family = stats::poisson()
    )

  testthat::expect_s3_class(mod, "gam")
  testthat::expect_equal(mod$family$family, "poisson")

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 5,
      error_family = "gaussian"
    ),
    "error_family must be a valid family object"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 5,
      error_family = list()
    ),
    "error_family must be a valid family object"
  )
})

testthat::test_that("fit_regression_model() returns a GAM model object", {
  set.seed(19900723)
  df <-
    data.frame(
      richness = rnorm(30),
      age = rep(seq(0, 1, length.out = 10), 3),
      dataset_id = factor(rep(letters[1:3], each = 10))
    )

  mod <-
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      random = "intercept",
      sel_k = 5
    )

  testthat::expect_s3_class(mod, "gam")
  testthat::expect_true(!is.null(mod$fitted.values))
  testthat::expect_equal(length(mod$fitted.values), nrow(df))
})

testthat::test_that("fit_regression_model() builds correct formulas for different random structures", {
  set.seed(19900723)
  df <-
    data.frame(
      richness = rnorm(30),
      age = rep(seq(0, 1, length.out = 10), 3),
      dataset_id = factor(rep(letters[1:3], each = 10))
    )

  mod_intercept <-
    fit_regression_model(
      data_source = df,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      random = "intercept",
      sel_k = 4
    )

  mod_slope <- fit_regression_model(
    data_source = df,
    y_var = "richness",
    time_var = "age",
    group_var = "dataset_id",
    random = "slope",
    sel_k = 4
  )

  mod_both <- fit_regression_model(
    data_source = df,
    y_var = "richness",
    time_var = "age",
    group_var = "dataset_id",
    random = "both",
    sel_k = 4
  )

  f_intercept <- stats::formula(mod_intercept)
  f_slope <- stats::formula(mod_slope)
  f_both <- stats::formula(mod_both)

  rhs_intercept <- as.character(f_intercept)[3]
  rhs_slope <- as.character(f_slope)[3]
  rhs_both <- as.character(f_both)[3]

  testthat::expect_true(
    stringr::str_detect(
      rhs_intercept,
      stringr::fixed("s(age, k = 4, bs = \"tp\")")
    )
  )
  expect_true(
    stringr::str_detect(
      rhs_intercept,
      stringr::regex("s\\(dataset_id, bs = \"re\"\\)")
    )
  )
  expect_false(
    stringr::str_detect(
      rhs_intercept,
      stringr::fixed("by = dataset_id")
    )
  )

  expect_true(
    stringr::str_detect(
      rhs_slope,
      stringr::regex("s\\(age, k = 4, bs = \"tp\"\\)")
    )
  )
  expect_true(
    stringr::str_detect(
      rhs_slope,
      stringr::regex("s\\(age, by = dataset_id, k = 4, bs = \"fs\"\\)")
    )
  )
  expect_false(
    stringr::str_detect(
      rhs_slope,
      stringr::regex("s\\(dataset_id, bs = \"re\"\\)")
    )
  )

  expect_true(
    stringr::str_detect(
      rhs_both,
      stringr::regex("s\\(age, k = 4, bs = \"tp\"\\)")
    )
  )
  expect_true(
    stringr::str_detect(
      rhs_both,
      stringr::regex("s\\(age, by = dataset_id, k = 4, bs = \"fs\"\\)")
    )
  )
  expect_true(
    stringr::str_detect(
      rhs_both,
      stringr::regex("s\\(dataset_id, bs = \"re\"\\)")
    )
  )
})

test_that("fit_regression_model() handles simple edge cases sensibly", {
  df_single_group <-
    data.frame(
      richness = rnorm(10),
      age = 1:10,
      dataset_id = factor(rep("A", 10))
    )

  mod_single_group <-
    fit_regression_model(
      data_source = df_single_group,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      random = "slope",
      sel_k = 3
    )

  expect_s3_class(mod_single_group, "gam")

  df_with_na <-
    data.frame(
      richness = c(rnorm(9), NA_real_),
      age = 1:10,
      dataset_id = rep(letters[1:2], length.out = 10) |>
        as.factor()
    )

  mod_with_na <-
    fit_regression_model(
      data_source = df_with_na,
      y_var = "richness",
      time_var = "age",
      group_var = "dataset_id",
      sel_k = 3
    )

  expect_s3_class(mod_with_na, "gam")
})
