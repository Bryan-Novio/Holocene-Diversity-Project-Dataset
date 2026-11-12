#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("predict_model() accepts valid inputs", {
  data_fit <-
    tibble::tibble(
      x = base::seq(from = -3, to = 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = base::sin(x)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x),
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      x = c(-1, 0, 1)
    )

  testthat::expect_no_error(
    res <-
      predict_model(
        model = mod,
        newdata = data_new
      )
  )

  testthat::expect_s3_class(res, "data.frame")
})

testthat::test_that("predict_model() validates model", {
  data_new <-
    tibble::tibble(
      x = c(-1, 0, 1)
    )

  mod_lm <-
    stats::lm(
      formula = mpg ~ wt,
      data = datasets::mtcars
    )

  testthat::expect_error(
    predict_model(
      model = "not a gam",
      newdata = data_new
    ),
    "`model` must be a 'gam' object\\."
  )

  testthat::expect_error(
    predict_model(
      model = mod_lm,
      newdata = data_new
    ),
    "`model` must be a 'gam' object\\."
  )

  testthat::expect_error(
    predict_model(
      model = NULL,
      newdata = data_new
    ),
    "`model` must be a 'gam' object\\."
  )
})

testthat::test_that("predict_model() validates newdata", {
  data_fit <-
    tibble::tibble(
      x = base::seq(from = -3, to = 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = base::sin(x)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x),
      data = data_fit
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
      newdata = base::list(x = 1:3)
    ),
    "`newdata` must be a data frame\\."
  )

  testthat::expect_error(
    predict_model(
      model = mod,
      newdata = NULL
    ),
    "`newdata` must be a data frame\\."
  )
})

testthat::test_that("predict_model() validates type", {
  data_fit <-
    tibble::tibble(
      x = base::seq(from = -3, to = 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = base::sin(x)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x),
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      x = c(-1, 0, 1)
    )

  list_bad_type <-
    base::list(
      1,
      c("response", "link"),
      character(0),
      NULL
    )

  for (
    sel_type in list_bad_type
  ) {
    testthat::expect_error(
      predict_model(
        model = mod,
        newdata = data_new,
        type = sel_type
      ),
      "`type` must be a single character string\\."
    )
  }

  testthat::expect_error(
    predict_model(
      model = mod,
      newdata = data_new,
      type = "not_a_valid_type"
    )
  )
})

testthat::test_that("predict_model() validates exclude_terms", {
  data_fit <-
    tibble::tibble(
      x = base::seq(from = -3, to = 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = base::sin(x)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x),
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      x = c(-1, 0, 1)
    )

  list_bad_terms <-
    base::list(
      1:3,
      character(0),
      TRUE,
      NA_character_,
      c("dataset_id", "region")
    )

  for (
    sel_terms in list_bad_terms
  ) {
    testthat::expect_error(
      predict_model(
        model = mod,
        newdata = data_new,
        exclude_terms = sel_terms
      ),
      "`exclude_terms` must be NULL or a single character string\\."
    )
  }
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("predict_model() returns one tibble row per newdata row", {
  data_fit <-
    tibble::tibble(
      x = base::seq(from = -3, to = 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = base::sin(x)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x),
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      x = c(-2, -1, 0, 1, 2)
    )

  res <-
    predict_model(
      model = mod,
      newdata = data_new
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(base::nrow(res), base::nrow(data_new))
  testthat::expect_true(
    base::all(
      c("x", "estimate", "std_error", "conf_low", "conf_high") %in%
        base::colnames(res)
    )
  )
  testthat::expect_equal(dplyr::pull(res, x), dplyr::pull(data_new, x))
})

testthat::test_that("predict_model() cleans only prediction column names", {
  data_fit <-
    tibble::tibble(
      Age.BP = base::seq(from = -3, to = 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = base::sin(Age.BP)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(Age.BP),
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      Age.BP = c(-1, 0, 1)
    )

  res <-
    predict_model(
      model = mod,
      newdata = data_new
    )

  vec_pred_cols <-
    base::setdiff(
      base::colnames(res),
      base::colnames(data_new)
    )

  testthat::expect_true("Age.BP" %in% base::colnames(res))
  testthat::expect_false("age_bp" %in% base::colnames(res))
  testthat::expect_true(
    base::all(
      vec_pred_cols == janitor::make_clean_names(vec_pred_cols)
    )
  )
  testthat::expect_false(
    base::any(base::grepl("\\.", vec_pred_cols))
  )
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("predict_model() matches mgcv predictions", {
  data_fit <-
    tibble::tibble(
      x = base::seq(from = -3, to = 3, length.out = 60)
    ) |>
    dplyr::mutate(
      y = base::as.integer(x + base::sin(3 * x) > 0)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, k = 5),
      family = stats::binomial(),
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      x = base::seq(from = -2, to = 2, length.out = 7)
    )

  res_response <-
    predict_model(
      model = mod,
      newdata = data_new,
      type = "response"
    )

  res_link <-
    predict_model(
      model = mod,
      newdata = data_new,
      type = "link"
    )

  vec_expected_response <-
    stats::predict(
      object = mod,
      newdata = data_new,
      type = "response"
    ) |>
    base::as.vector()

  vec_expected_link <-
    stats::predict(
      object = mod,
      newdata = data_new,
      type = "link"
    ) |>
    base::as.vector()

  testthat::expect_equal(
    dplyr::pull(res_response, estimate),
    vec_expected_response,
    tolerance = 1e-8
  )
  testthat::expect_equal(
    dplyr::pull(res_link, estimate),
    vec_expected_link,
    tolerance = 1e-8
  )
  testthat::expect_true(
    base::all(
      dplyr::pull(res_response, estimate) >= 0 &
        dplyr::pull(res_response, estimate) <= 1
    )
  )
})

testthat::test_that("predict_model() drops smooths of exclude_terms", {
  base::set.seed(900723)

  data_fit <-
    tidyr::expand_grid(
      dataset_id = base::factor(base::seq_len(6)),
      x = base::seq(from = -2, to = 2, length.out = 15)
    ) |>
    dplyr::mutate(
      offset_id = stats::rnorm(n = 6, sd = 2)[base::as.integer(dataset_id)],
      y = base::sin(x) + offset_id + stats::rnorm(n = dplyr::n(), sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x) + s(dataset_id, bs = "re"),
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      x = c(-1, 0, 1)
    )

  res <-
    predict_model(
      model = mod,
      newdata = data_new,
      exclude_terms = "dataset_id"
    )

  data_new_with_id <-
    dplyr::mutate(
      data_new,
      dataset_id = base::factor(
        "1",
        levels = base::levels(dplyr::pull(data_fit, dataset_id))
      )
    )

  vec_expected <-
    stats::predict(
      object = mod,
      newdata = data_new_with_id,
      exclude = "s(dataset_id)"
    ) |>
    base::as.vector()

  vec_with_id <-
    stats::predict(
      object = mod,
      newdata = data_new_with_id
    ) |>
    base::as.vector()

  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_equal(
    dplyr::pull(res, estimate),
    vec_expected,
    tolerance = 1e-6
  )
  testthat::expect_true(
    base::any(base::abs(dplyr::pull(res, estimate) - vec_with_id) > 1e-3)
  )
})

testthat::test_that("predict_model() result ignores excluded factor level", {
  base::set.seed(900723)

  data_fit <-
    tidyr::expand_grid(
      dataset_id = base::factor(base::seq_len(6)),
      x = base::seq(from = -2, to = 2, length.out = 15)
    ) |>
    dplyr::mutate(
      offset_id = stats::rnorm(n = 6, sd = 2)[base::as.integer(dataset_id)],
      y = base::sin(x) + offset_id + stats::rnorm(n = dplyr::n(), sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x) + s(dataset_id, bs = "re"),
      data = data_fit
    )

  data_new_level_5 <-
    tibble::tibble(
      x = c(-1, 0, 1),
      dataset_id = base::factor("5", levels = base::as.character(1:6))
    )

  res_level_5 <-
    predict_model(
      model = mod,
      newdata = data_new_level_5,
      exclude_terms = "dataset_id"
    )

  # The population-level prediction from mgcv, made with level 5 in place
  vec_expected <-
    stats::predict(
      object = mod,
      newdata = data_new_level_5,
      exclude = "s(dataset_id)"
    ) |>
    base::as.vector()

  vec_level_5 <-
    stats::predict(
      object = mod,
      newdata = data_new_level_5
    ) |>
    base::as.vector()

  testthat::expect_equal(
    dplyr::pull(res_level_5, estimate),
    vec_expected,
    tolerance = 1e-6
  )
  vec_diff <-
    dplyr::pull(res_level_5, estimate) - vec_level_5

  testthat::expect_true(base::any(base::abs(vec_diff) > 1e-3))
})

testthat::test_that("predict_model() forwards arguments in ...", {
  data_fit <-
    tibble::tibble(
      x = base::seq(from = -3, to = 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = base::sin(x)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x),
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      x = c(-1, 0, 1)
    )

  res_95 <-
    predict_model(
      model = mod,
      newdata = data_new
    )

  res_50 <-
    predict_model(
      model = mod,
      newdata = data_new,
      conf_level = 0.5
    )

  vec_width_95 <-
    dplyr::pull(res_95, conf_high) - dplyr::pull(res_95, conf_low)

  vec_width_50 <-
    dplyr::pull(res_50, conf_high) - dplyr::pull(res_50, conf_low)

  testthat::expect_equal(
    dplyr::pull(res_95, estimate),
    dplyr::pull(res_50, estimate),
    tolerance = 1e-8
  )
  testthat::expect_true(base::all(vec_width_50 < vec_width_95))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("predict_model() handles a single-row newdata", {
  data_fit <-
    tibble::tibble(
      x = base::seq(from = -3, to = 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = base::sin(x)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x),
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      x = 0.5
    )

  res <-
    predict_model(
      model = mod,
      newdata = data_new
    )

  testthat::expect_equal(base::nrow(res), 1L)
})

testthat::test_that("predict_model() errors for unknown exclude_terms", {
  data_fit <-
    tibble::tibble(
      x = base::seq(from = -3, to = 3, length.out = 50)
    ) |>
    dplyr::mutate(
      y = base::sin(x)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x),
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      x = c(-1, 0, 1)
    )

  testthat::expect_error(
    predict_model(
      model = mod,
      newdata = data_new,
      exclude_terms = "dataset_id"
    ),
    "No predictor variables found for term 'dataset_id'"
  )
})

testthat::test_that("predict_model() errors when a predictor is missing", {
  data_fit <-
    tibble::tibble(
      x = base::seq(from = -3, to = 3, length.out = 50),
      z = base::rev(x)
    ) |>
    dplyr::mutate(
      y = base::sin(x) + z
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x) + z,
      data = data_fit
    )

  data_new <-
    tibble::tibble(
      x = c(-1, 0, 1)
    )

  testthat::expect_error(
    predict_model(
      model = mod,
      newdata = data_new
    )
  )
})
