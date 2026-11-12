#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("extract_factor_levels() accepts valid inputs", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 40),
      group = base::factor(base::rep(c("A", "B", "C", "D"), times = 10))
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 40, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, by = group),
      data = data_fit
    )

  testthat::expect_no_error(
    res <-
      extract_factor_levels(
        model = mod,
        sel_term = "group"
      )
  )

  testthat::expect_length(res, 4L)
})

testthat::test_that("extract_factor_levels() validates model", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 40),
      group = base::factor(base::rep(c("A", "B"), times = 20))
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 40, sd = 0.1)
    )

  mod_lm <-
    stats::lm(
      formula = y ~ x,
      data = data_fit
    )

  list_bad_model <-
    base::list(
      "not_a_model",
      mod_lm,
      base::list(),
      NULL
    )

  for (
    sel_model in list_bad_model
  ) {
    testthat::expect_error(
      extract_factor_levels(
        model = sel_model,
        sel_term = "group"
      ),
      "`model` must be a GAM/BAM object\\."
    )
  }
})

testthat::test_that("extract_factor_levels() validates sel_term", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 40),
      group = base::factor(base::rep(c("A", "B"), times = 20))
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 40, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, by = group),
      data = data_fit
    )

  list_bad_term <-
    base::list(
      c("group", "x"),
      123,
      TRUE,
      NULL,
      base::factor("group"),
      NA_character_,
      character(0)
    )

  for (
    sel_bad in list_bad_term
  ) {
    testthat::expect_error(
      extract_factor_levels(
        model = mod,
        sel_term = sel_bad
      ),
      "`sel_term` must be a single character string\\."
    )
  }
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("extract_factor_levels() returns a factor for a factor", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 60),
      group = base::factor(base::rep(c("A", "B", "C"), times = 20))
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 60, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, by = group),
      data = data_fit
    )

  res <-
    extract_factor_levels(
      model = mod,
      sel_term = "group"
    )

  testthat::expect_s3_class(res, "factor")
  testthat::expect_false(base::is.list(res))
  testthat::expect_equal(base::levels(res), c("A", "B", "C"))
})

testthat::test_that("extract_factor_levels() works with bam models", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 90),
      category = base::factor(base::rep(c("cat1", "cat2", "cat3"), times = 30))
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 90, sd = 0.1)
    )

  mod <-
    mgcv::bam(
      formula = y ~ s(x, by = category),
      data = data_fit
    )

  res <-
    extract_factor_levels(
      model = mod,
      sel_term = "category"
    )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_s3_class(res, "factor")
  testthat::expect_equal(base::as.character(res), c("cat1", "cat2", "cat3"))
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("extract_factor_levels() keeps order of appearance", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 60),
      group = base::factor(
        base::rep(c("C", "A", "B"), times = 20),
        levels = c("A", "B", "C")
      )
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 60, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, by = group),
      data = data_fit
    )

  res <-
    extract_factor_levels(
      model = mod,
      sel_term = "group"
    )

  vec_expected <-
    base::factor(
      c("C", "A", "B"),
      levels = c("A", "B", "C")
    )

  testthat::expect_equal(res, vec_expected)
})

testthat::test_that("extract_factor_levels() returns distinct values only", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 120),
      stage = base::factor(base::rep(c("early", "mid", "late"), times = 40))
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 120, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, by = stage),
      data = data_fit
    )

  res <-
    extract_factor_levels(
      model = mod,
      sel_term = "stage"
    )

  testthat::expect_equal(base::length(res), 3L)
  testthat::expect_equal(
    base::as.character(res),
    c("early", "mid", "late")
  )
  testthat::expect_identical(
    res,
    extract_factor_levels(
      model = mod,
      sel_term = "stage"
    )
  )
})

testthat::test_that("extract_factor_levels() selects the requested term", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x1 = stats::rnorm(n = 60),
      x2 = stats::rnorm(n = 60),
      region = base::factor(base::rep(c("G1", "G2"), times = 30)),
      habitat = base::factor(base::rep(c("H1", "H2", "H3"), times = 20))
    ) |>
    dplyr::mutate(
      y = base::sin(x1) + stats::rnorm(n = 60, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x1, by = region) + s(x2, by = habitat),
      data = data_fit
    )

  res_region <-
    extract_factor_levels(
      model = mod,
      sel_term = "region"
    )

  res_habitat <-
    extract_factor_levels(
      model = mod,
      sel_term = "habitat"
    )

  testthat::expect_equal(base::as.character(res_region), c("G1", "G2"))
  testthat::expect_equal(
    base::as.character(res_habitat),
    c("H1", "H2", "H3")
  )
})

testthat::test_that("extract_factor_levels() matches by prefix, any case", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 60),
      group_type = base::factor(base::rep(c("t1", "t2", "t3"), times = 20))
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 60, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, by = group_type),
      data = data_fit
    )

  res_prefix <-
    extract_factor_levels(
      model = mod,
      sel_term = "group"
    )

  res_full <-
    extract_factor_levels(
      model = mod,
      sel_term = "group_type"
    )

  res_case <-
    extract_factor_levels(
      model = mod,
      sel_term = "GROUP"
    )

  testthat::expect_equal(res_prefix, res_full)
  testthat::expect_equal(res_case, res_full)
  testthat::expect_equal(base::as.character(res_full), c("t1", "t2", "t3"))
})

testthat::test_that("extract_factor_levels() returns numeric predictors", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = c(3, 1, 2, 3, 1, 2, 3, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 20, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, k = 5),
      data = data_fit
    )

  res <-
    extract_factor_levels(
      model = mod,
      sel_term = "x"
    )

  testthat::expect_type(res, "double")
  testthat::expect_equal(res, c(3, 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
})

testthat::test_that("extract_factor_levels() keeps special characters", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 80),
      code = base::factor(base::rep(c("A-1", "B 2", "C.3", "D/4"), times = 20))
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 80, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, by = code),
      data = data_fit
    )

  res <-
    extract_factor_levels(
      model = mod,
      sel_term = "code"
    )

  testthat::expect_equal(
    base::as.character(res),
    c("A-1", "B 2", "C.3", "D/4")
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("extract_factor_levels() errors when no term matches", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 40),
      group = base::factor(base::rep(c("A", "B"), times = 20))
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 40, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, by = group),
      data = data_fit
    )

  testthat::expect_error(
    extract_factor_levels(
      model = mod,
      sel_term = "nonexistent"
    ),
    "No predictor variables found for term 'nonexistent'\\."
  )
})

testthat::test_that("extract_factor_levels() errors on several matches", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x1 = stats::rnorm(n = 60),
      x2 = stats::rnorm(n = 60),
      group1 = base::factor(base::rep(c("G1", "G2"), times = 30)),
      group2 = base::factor(base::rep(c("H1", "H2", "H3"), times = 20))
    ) |>
    dplyr::mutate(
      y = base::sin(x1) + stats::rnorm(n = 60, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x1, by = group1) + s(x2, by = group2),
      data = data_fit
    )

  testthat::expect_error(
    extract_factor_levels(
      model = mod,
      sel_term = "group"
    ),
    "Multiple predictor variables found for term 'group'\\."
  )
})

testthat::test_that("extract_factor_levels() handles a one-level factor", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      x = stats::rnorm(n = 40),
      group = base::factor(base::rep("only", times = 40))
    ) |>
    dplyr::mutate(
      y = base::sin(x) + stats::rnorm(n = 40, sd = 0.1)
    )

  mod <-
    mgcv::gam(
      formula = y ~ s(x, by = group),
      data = data_fit
    )

  res <-
    extract_factor_levels(
      model = mod,
      sel_term = "group"
    )

  testthat::expect_s3_class(res, "factor")
  testthat::expect_equal(base::as.character(res), "only")
})
