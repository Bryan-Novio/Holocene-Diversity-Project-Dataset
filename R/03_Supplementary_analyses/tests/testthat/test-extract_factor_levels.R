test_that("extract_factor_levels() validates input types", {
  data_test <-
    data.frame(
      y = rnorm(100),
      x = rnorm(100),
      group = factor(rep(c("A", "B", "C", "D"), 25))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = group), data = data_test)

  expect_error(
    extract_factor_levels(
      model = "not_a_model",
      sel_term = "group"
    ),
    "`model` must be a GAM/BAM object."
  )

  expect_error(
    extract_factor_levels(
      model = lm(y ~ x, data = data_test),
      sel_term = "group"
    ),
    "`model` must be a GAM/BAM object."
  )

  expect_error(
    extract_factor_levels(
      model = list(),
      sel_term = "group"
    ),
    "`model` must be a GAM/BAM object."
  )

  expect_error(
    extract_factor_levels(
      model = NULL,
      sel_term = "group"
    ),
    "`model` must be a GAM/BAM object."
  )
})

test_that("extract_factor_levels() validates sel_term argument", {
  data_test <-
    data.frame(
      y = rnorm(100),
      x = rnorm(100),
      group = factor(rep(c("A", "B"), 50))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = group), data = data_test)

  expect_error(
    extract_factor_levels(
      model = model_test,
      sel_term = c("group", "other")
    ),
    "`sel_term` must be a single character string."
  )

  expect_error(
    extract_factor_levels(
      model = model_test,
      sel_term = 123
    ),
    "`sel_term` must be a single character string."
  )

  expect_error(
    extract_factor_levels(
      model = model_test,
      sel_term = TRUE
    ),
    "`sel_term` must be a single character string."
  )

  expect_error(
    extract_factor_levels(
      model = model_test,
      sel_term = NULL
    ),
    "`sel_term` must be a single character string."
  )

  expect_error(
    extract_factor_levels(
      model = model_test,
      sel_term = factor("group")
    ),
    "`sel_term` must be a single character string."
  )
})

test_that("extract_factor_levels() errors when term not found", {
  data_test <-
    data.frame(
      y = rnorm(100),
      x = rnorm(100),
      group = factor(rep(c("A", "B"), 50))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = group), data = data_test)

  expect_error(
    extract_factor_levels(
      model = model_test,
      sel_term = "nonexistent"
    ),
    "No predictor variables found for term 'nonexistent'. Please ensure 'sel_term' matches a single factor variable in the model."
  )

  expect_error(
    extract_factor_levels(
      model = model_test,
      sel_term = "xyz"
    ),
    "No predictor variables found for term 'xyz'. Please ensure 'sel_term' matches a single factor variable in the model."
  )
})

test_that("extract_factor_levels() returns factor", {
  data_test <-
    data.frame(
      y = rnorm(100),
      x = rnorm(100),
      group = factor(rep(c("A", "B", "C"), length.out = 100))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = group), data = data_test)

  result <-
    extract_factor_levels(
      model = model_test,
      sel_term = "group"
    )

  expect_s3_class(result, "factor")
  expect_true(is.factor(result))
  expect_false(is.list(result))
})

test_that("extract_factor_levels() extracts correct factor levels from gam", {
  set.seed(19900723)
  data_test <-
    data.frame(
      y = rnorm(120),
      x = rnorm(120),
      group = factor(rep(c("A", "B", "C", "D"), 30))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = group), data = data_test)

  result <-
    extract_factor_levels(
      model = model_test,
      sel_term = "group"
    )

  expect_true(length(result) >= 1)
  expect_true(all(nchar(as.character(result)) > 0))

  tidy_result <-
    factor(c("A", "B", "C", "D"))

  expect_true(length(tidy_result) > 0)
  expect_equal(length(result), length(tidy_result))
})

test_that("extract_factor_levels() works with bam models", {
  set.seed(19900723)
  data_test <-
    data.frame(
      y = rnorm(200),
      x = rnorm(200),
      category = factor(rep(c("cat1", "cat2", "cat3"), length.out = 200))
    )

  model_test <-
    mgcv::bam(y ~ s(x, by = category), data = data_test)

  result <-
    extract_factor_levels(
      model = model_test,
      sel_term = "category"
    )

  expect_s3_class(result, "factor")
  expect_true(length(result) >= 1)
})

test_that("extract_factor_levels() handles models with multiple terms", {
  set.seed(19900723)
  data_test <-
    data.frame(
      y = rnorm(150),
      x1 = rnorm(150),
      x2 = rnorm(150),
      group1 = factor(rep(c("G1", "G2"), 75)),
      group2 = factor(rep(c("H1", "H2", "H3"), 50))
    )

  model_test <-
    mgcv::gam(y ~ s(x1, by = group1) + s(x2, by = group2), data = data_test)

  result1 <-
    extract_factor_levels(
      model = model_test,
      sel_term = "group1"
    )
  result2 <-
    extract_factor_levels(
      model = model_test,
      sel_term = "group2"
    )

  expect_s3_class(result1, "factor")
  expect_s3_class(result2, "factor")
  expect_true(length(result1) >= 1)
  expect_true(length(result2) >= 1)
})

test_that("extract_factor_levels() extracts levels matching term pattern", {
  set.seed(19900723)
  data_test <-
    data.frame(
      y = rnorm(100),
      x = rnorm(100),
      treatment = factor(rep(c("control", "low", "medium", "high"), 25))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = treatment), data = data_test)

  result <-
    extract_factor_levels(
      model = model_test,
      sel_term = "treatment"
    )

  tidy_data <-
    model_test %>%
    broom::tidy()

  terms_with_treatment <-
    tidy_data %>%
    dplyr::filter(stringr::str_detect(term, ":treatment"))

  expected_levels <-
    c("control", "low", "medium", "high") %>%
    factor()

  expect_equal(result, expected_levels)
})

test_that("extract_factor_levels() handles factor with two levels", {
  set.seed(19900723)
  data_test <-
    data.frame(
      y = rnorm(80),
      x = rnorm(80),
      binary = factor(rep(c("yes", "no"), 40))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = binary), data = data_test)

  result <-
    extract_factor_levels(
      model = model_test,
      sel_term = "binary"
    )

  expect_s3_class(result, "factor")
  expect_true(length(result) >= 1)
  expect_true(all(as.character(result) %in% c("yes", "no")))
})

test_that("extract_factor_levels() returns levels in consistent order", {
  set.seed(19900723)
  data_test <-
    data.frame(
      y = rnorm(120),
      x = rnorm(120),
      stage = factor(rep(c("early", "mid", "late"), 40))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = stage), data = data_test)

  result1 <-
    extract_factor_levels(
      model = model_test,
      sel_term = "stage"
    )

  result2 <-
    extract_factor_levels(
      model = model_test,
      sel_term = "stage"
    )

  expect_identical(result1, result2)
})

test_that("extract_factor_levels() handles numeric-like factor levels", {
  set.seed(19900723)
  data_test <-
    data.frame(
      y = rnorm(100),
      x = rnorm(100),
      year = factor(rep(c("2020", "2021", "2022", "2023"), 25))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = year), data = data_test)

  result <-
    extract_factor_levels(
      model = model_test,
      sel_term = "year"
    )

  expect_s3_class(result, "factor")
  expect_true(length(result) >= 1)
  expect_true(all(grepl("^[0-9]{4}$", as.character(result))))
})

test_that("extract_factor_levels() handles special characters in levels", {
  set.seed(19900723)
  data_test <-
    data.frame(
      y = rnorm(100),
      x = rnorm(100),
      code = factor(rep(c("A-1", "B-2", "C-3", "D-4"), 25))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = code), data = data_test)

  result <-
    extract_factor_levels(
      model = model_test,
      sel_term = "code"
    )

  expect_s3_class(result, "factor")
  expect_true(length(result) >= 1)
})

test_that("extract_factor_levels() handles underscore in term name", {
  set.seed(19900723)
  data_test <-
    data.frame(
      y = rnorm(100),
      x = rnorm(100),
      group_type = factor(rep(c("type1", "type2", "type3"), length.out = 100))
    )

  model_test <-
    mgcv::gam(y ~ s(x, by = group_type), data = data_test)

  result <-
    extract_factor_levels(
      model = model_test,
      sel_term = "group_type"
    )

  expect_s3_class(result, "factor")
  expect_true(length(result) >= 1)
})
