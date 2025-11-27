testthat::test_that("predict_model() validates inputs", {
  testthat::skip_if_not_installed("mgcv")
  testthat::skip_if_not_installed("marginaleffects")
  testthat::skip_if_not_installed("gratia")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not_installed("tibble")

  df <-
    tibble::tibble(
      x = seq(-3, 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = as.numeric(x > 0)
    )

  mod <-
    mgcv::gam(y ~ s(x), family = binomial, data = df)

  newdata <-
    tibble::tibble(x = c(-1, 0, 1))

  testthat::expect_no_error(
    predict_model(
      mod,
      newdata = newdata
    )
  )

  testthat::expect_error(
    predict_model(
      model = "not a gam",
      newdata = newdata
    ),
    "`model` must be a 'gam' object\\."
  )

  testthat::expect_error(
    predict_model(
      model = mod,
      newdata = 1:3
    ),
    "`newdata` must be a data frame\\."
  )

  testthat::expect_error(
    predict_model(
      model = mod,
      newdata = newdata, type = 1
    ),
    "`type` must be a single character string\\."
  )

  testthat::expect_error(
    predict_model(
      model = mod,
      newdata = newdata,
      type = c("response", "link")
    ),
    "`type` must be a single character string\\."
  )

  testthat::expect_error(
    predict_model(
      model = mod,
      newdata = newdata,
      exclude_terms = 1:3
    ),
    "`exclude_terms` must be NULL or a character vector\\."
  )

  testthat::expect_error(
    predict_model(
      model = mod,
      newdata = newdata,
      exclude_terms = character(0)
    ),
    "`exclude_terms` must be NULL or a character vector\\."
  )
})

testthat::test_that("predict_model() returns a tibble with expected structure", {
  testthat::skip_if_not_installed("mgcv")
  testthat::skip_if_not_installed("marginaleffects")
  testthat::skip_if_not_installed("gratia")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not_installed("tibble")

  df <-
    tibble::tibble(
      x = seq(-3, 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = as.numeric(x > 0)
    )

  mod <-
    mgcv::gam(y ~ s(x), family = binomial, data = df)

  newdata <-
    tibble::tibble(
      x = c(-2, -1, 0, 1, 2)
    )

  res <-
    predict_model(
      mod,
      newdata = newdata
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true(is.data.frame(res))
  testthat::expect_equal(
    nrow(res),
    nrow(newdata)
  )
  testthat::expect_gt(ncol(res), 0)
  testthat::expect_true(
    "estimate" %in% names(res)
  )
})

testthat::test_that("predict_model() respects the 'type' argument", {
  testthat::skip_if_not_installed("mgcv")
  testthat::skip_if_not_installed("marginaleffects")
  testthat::skip_if_not_installed("gratia")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not_installed("tibble")
  testthat::skip_if_not_installed("dplyr")


  df <-
    tibble::tibble(
      x = seq(-3, 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = as.numeric(x > 0)
    )

  mod <-
    mgcv::gam(y ~ s(x), family = binomial, data = df)

  newdata <-
    tibble::tibble(
      x = seq(-2, 2, length.out = 10)
    )

  res_response <-
    predict_model(mod, newdata = newdata, type = "response")

  res_link <-
    predict_model(mod, newdata = newdata, type = "link")

  testthat::expect_equal(
    nrow(res_response),
    nrow(res_link)
  )
  testthat::expect_true(
    "estimate" %in% names(res_response)
  )
  testthat::expect_true(
    "estimate" %in% names(res_link)
  )
  testthat::expect_true(
    any(
      abs(res_response$estimate - res_link$estimate) > 1e-6
    )
  )

  testthat::expect_error(
    predict_model(
      mod,
      newdata = newdata,
      type = "not_a_valid_type"
    )
  )
})

testthat::test_that("predict_model() uses exclude_terms to alter predictions", {
  testthat::skip_if_not_installed("mgcv")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")
  testthat::skip_if_not_installed("marginaleffects")
  testthat::skip_if_not_installed("gratia")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not_installed("tibble")

  set.seed(19900723)
  df <-
    tidyr::expand_grid(
      x = seq(-2, 2, length.out = 20),
      id = factor(1:5),
      z = seq(-1, 1, length.out = 5)
    ) |>
    dplyr::mutate(
      random_intercept = rnorm(dplyr::n(), mean = 0, sd = 1)[as.numeric(id)],
      random_slope = rnorm(dplyr::n(), mean = 0, sd = 0.5)[as.numeric(id)],
      eta = x + random_slope * x + random_intercept + 2 * z,
      y = as.numeric(eta > 0)
    )

  mod <-
    mgcv::gam(y ~ s(x, by = id) + s(id, bs = "re"), family = binomial, data = df)

  newdata <-
    tidyr::expand_grid(
      x = seq(-2, 2, length.out = 10),
      id = factor(1),
      z = seq(-1, 1, length.out = 5)
    )

  pred_full <-
    predict_model(
      mod,
      newdata = newdata,
      type = "response"
    )

  pred_excl_id <-
    predict_model(
      mod,
      newdata = newdata,
      type = "response",
      exclude_terms = "id"
    )

  testthat::expect_equal(
    nrow(pred_full),
    nrow(pred_excl_id)
  )
  testthat::expect_true(
    "estimate" %in% names(pred_full)
  )
  testthat::expect_true(
    "estimate" %in% names(pred_excl_id)
  )
  testthat::expect_true(
    any(
      abs(pred_full$estimate - pred_excl_id$estimate) > 1e-6
    )
  )
})

testthat::test_that("predict_model() forwards additional arguments in ...", {
  testthat::skip_if_not_installed("mgcv")
  testthat::skip_if_not_installed("marginaleffects")
  testthat::skip_if_not_installed("gratia")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not_installed("tibble")

  df <-
    tibble::tibble(
      x = seq(-3, 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = as.numeric(x > 0)
    )

  mod <-
    mgcv::gam(y ~ s(x), family = binomial, data = df)

  newdata <-
    tibble::tibble(
      x = seq(-2, 2, length.out = 8)
    )

  res_default <-
    predict_model(
      mod,
      newdata = newdata,
      type = "response"
    )

  res_vcov_false <-
    predict_model(
      mod,
      newdata = newdata,
      type = "response",
      vcov = FALSE
    )

  testthat::expect_s3_class(
    res_vcov_false, "tbl_df"
  )
  testthat::expect_equal(
    nrow(res_default),
    nrow(res_vcov_false)
  )
})

testthat::test_that("predict_model() handles zero-row newdata gracefully", {
  testthat::skip_if_not_installed("mgcv")
  testthat::skip_if_not_installed("marginaleffects")
  testthat::skip_if_not_installed("gratia")
  testthat::skip_if_not_installed("stringr")
  testthat::skip_if_not_installed("tibble")

  df <-
    tibble::tibble(
      x = seq(-3, 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = as.numeric(x > 0)
    )

  mod <-
    mgcv::gam(y ~ s(x), family = binomial, data = df)

  newdata <-
    tibble::tibble(x = numeric(0))

  res <-
    predict_model(mod,
      newdata = newdata
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true(is.data.frame(res))
  testthat::expect_equal(nrow(res), 0)
})
