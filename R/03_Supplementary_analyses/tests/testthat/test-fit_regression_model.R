#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("fit_regression_model() accepts valid inputs", {
  base::set.seed(900723)

  data_fit <-
    tidyr::expand_grid(
      dataset_id = base::factor(base::seq_len(6)),
      age = base::seq(from = 0, to = 1, length.out = 12)
    ) |>
    dplyr::mutate(
      region = base::factor(
        dplyr::if_else(base::as.integer(dataset_id) <= 3L, "A", "B")
      ),
      richness = base::sin(3 * age) + stats::rnorm(n = dplyr::n(), sd = 0.2)
    )

  testthat::expect_no_error(
    mod <-
      fit_regression_model(
        data_source = data_fit,
        y_var = "richness",
        time_var = "age",
        group_var = "region",
        random = "intercept",
        sel_k = 4
      )
  )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_equal(stats::nobs(mod), base::nrow(data_fit))
})

testthat::test_that("fit_regression_model() validates data_source", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      richness = stats::rnorm(n = 24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12))
    )

  list_bad_data <-
    base::list(
      base::as.matrix(data_fit),
      base::as.list(data_fit),
      "data_fit",
      NULL
    )

  for (
    sel_data in list_bad_data
  ) {
    testthat::expect_error(
      fit_regression_model(
        data_source = sel_data,
        y_var = "richness",
        time_var = "age",
        group_var = "region",
        random = "intercept",
        sel_k = 4
      ),
      "data_source must be a data\\.frame"
    )
  }
})

testthat::test_that("fit_regression_model() validates y_var", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12))
    )

  list_bad_var <-
    base::list(
      1,
      c("richness", "age"),
      NA_character_,
      character(0),
      NULL
    )

  for (
    sel_bad in list_bad_var
  ) {
    testthat::expect_error(
      fit_regression_model(
        data_source = data_fit,
        y_var = sel_bad,
        time_var = "age",
        group_var = "region",
        random = "intercept",
        sel_k = 4
      ),
      "y_var must be a single character string"
    )
  }
})

testthat::test_that("fit_regression_model() validates time_var", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12))
    )

  list_bad_var <-
    base::list(
      1,
      c("age", "richness"),
      NA_character_,
      character(0),
      NULL
    )

  for (
    sel_bad in list_bad_var
  ) {
    testthat::expect_error(
      fit_regression_model(
        data_source = data_fit,
        y_var = "richness",
        time_var = sel_bad,
        group_var = "region",
        random = "intercept",
        sel_k = 4
      ),
      "time_var must be a single character string"
    )
  }
})

testthat::test_that("fit_regression_model() validates group_var", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12))
    )

  list_bad_var <-
    base::list(
      1,
      c("region", "age"),
      NA_character_,
      character(0),
      NULL
    )

  for (
    sel_bad in list_bad_var
  ) {
    testthat::expect_error(
      fit_regression_model(
        data_source = data_fit,
        y_var = "richness",
        time_var = "age",
        group_var = sel_bad,
        random = "intercept",
        sel_k = 4
      ),
      "group_var must be a single character string"
    )
  }
})

testthat::test_that("fit_regression_model() validates variable columns", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12))
    )

  testthat::expect_error(
    fit_regression_model(
      data_source = data_fit,
      y_var = "not_in_data",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4
    ),
    "data_source must be a containing the variables: not_in_data"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "not_in_data",
      group_var = "region",
      random = "intercept",
      sel_k = 4
    ),
    "data_source must be a containing the variables"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "not_in_data",
      random = "intercept",
      sel_k = 4
    ),
    "data_source must be a containing the variables"
  )

  data_no_richness <-
    dplyr::select(
      data_fit,
      -richness
    )

  testthat::expect_error(
    fit_regression_model(
      data_source = data_no_richness,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4
    ),
    "data_source must be a containing the variables"
  )
})

testthat::test_that("fit_regression_model() validates random", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12))
    )

  testthat::expect_error(
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "something_else",
      sel_k = 4
    ),
    "'arg' should be one of"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = c("intercept", "slope"),
      sel_k = 4
    ),
    "'arg' must be of length 1"
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = 1,
      sel_k = 4
    ),
    "'arg' must be NULL or a character vector"
  )
})

testthat::test_that("fit_regression_model() requires dataset_id if needed", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12))
    )

  vec_options <-
    c("slope_rand_id", "slope_rand_id_2", "intercept_reg", "hgam_GI")

  for (
    sel_random in vec_options
  ) {
    testthat::expect_error(
      fit_regression_model(
        data_source = data_fit,
        y_var = "richness",
        time_var = "age",
        group_var = "region",
        random = sel_random,
        sel_k = 4
      ),
      "data_source must contain 'dataset_id'"
    )
  }
})

testthat::test_that("fit_regression_model() requires a factor group_var", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      dataset_id = base::rep(c(1155, 1263), each = 12)
    )

  data_character <-
    dplyr::mutate(
      data_fit,
      dataset_id = base::as.character(dataset_id)
    )

  for (
    sel_data in base::list(data_fit, data_character)
  ) {
    testthat::expect_error(
      fit_regression_model(
        data_source = sel_data,
        y_var = "richness",
        time_var = "age",
        group_var = "dataset_id",
        random = "intercept",
        sel_k = 4
      ),
      "the group_var column must be a factor"
    )
  }
})

testthat::test_that("fit_regression_model() requires a factor dataset_id", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12)),
      dataset_id = base::rep(c(1, 2, 3, 4), each = 6)
    )

  vec_options <-
    c("slope_rand_id", "slope_rand_id_2", "intercept_reg", "hgam_GI")

  for (
    sel_random in vec_options
  ) {
    testthat::expect_error(
      fit_regression_model(
        data_source = data_fit,
        y_var = "richness",
        time_var = "age",
        group_var = "region",
        random = sel_random,
        sel_k = 4
      ),
      "the dataset_id column must be a factor"
    )
  }
})

testthat::test_that("fit_regression_model() rejects reserved ... arguments", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12))
    )

  testthat::expect_error(
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4,
      method = "REML"
    ),
    "formula, data, method, and family cannot be passed in \\.\\.\\."
  )

  testthat::expect_error(
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4,
      family = stats::poisson()
    ),
    "cannot be passed in"
  )
})

testthat::test_that("fit_regression_model() validates sel_k", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12))
    )

  list_bad_k <-
    base::list(
      "3",
      c(3, 4),
      0,
      -1,
      NA_real_,
      NULL,
      numeric(0),
      TRUE
    )

  for (
    sel_bad in list_bad_k
  ) {
    testthat::expect_error(
      fit_regression_model(
        data_source = data_fit,
        y_var = "richness",
        time_var = "age",
        group_var = "region",
        random = "intercept",
        sel_k = sel_bad
      ),
      "sel_k must be a single positive numeric value"
    )
  }

  testthat::expect_error(
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept"
    )
  )
})

testthat::test_that("fit_regression_model() validates error_family", {
  data_fit <-
    tibble::tibble(
      richness = base::seq_len(24),
      age = base::rep(base::seq_len(12), times = 2),
      region = base::factor(base::rep(c("A", "B"), each = 12))
    )

  list_bad_family <-
    base::list(
      "gaussian",
      base::list(),
      stats::gaussian,
      1,
      NULL
    )

  for (
    sel_bad in list_bad_family
  ) {
    testthat::expect_error(
      fit_regression_model(
        data_source = data_fit,
        y_var = "richness",
        time_var = "age",
        group_var = "region",
        random = "intercept",
        sel_k = 4,
        error_family = sel_bad
      ),
      "error_family must be a valid family object"
    )
  }
})

testthat::test_that("fit_regression_model() fits with the default random", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      richness = stats::rnorm(n = 48),
      age = base::rep(base::seq(from = 0, to = 1, length.out = 12), times = 4),
      region = base::factor(base::rep(c("A", "B", "C", "D"), each = 12))
    )

  mod <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      sel_k = 4
    )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_equal(gratia::smooths(mod), c("s(age)", "s(region)"))
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("fit_regression_model() returns a bam fitted by fREML", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      richness = stats::rnorm(n = 48),
      age = base::rep(base::seq(from = 0, to = 1, length.out = 12), times = 4),
      region = base::factor(base::rep(c("A", "B", "C", "D"), each = 12))
    )

  mod <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4
    )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_s3_class(mod, "gam")
  testthat::expect_equal(mod[["method"]], "fREML")
  testthat::expect_length(stats::fitted(mod), base::nrow(data_fit))
})

testthat::test_that("fit_regression_model() builds the intercept model", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      richness = stats::rnorm(n = 48),
      age = base::rep(base::seq(from = 0, to = 1, length.out = 12), times = 4),
      region = base::factor(base::rep(c("A", "B", "C", "D"), each = 12))
    )

  mod <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4
    )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_equal(gratia::smooths(mod), c("s(age)", "s(region)"))
  testthat::expect_equal(mod[["smooth"]][[1]][["bs.dim"]], 4L)
  testthat::expect_s3_class(mod[["smooth"]][[1]], "cr.smooth")
  testthat::expect_s3_class(mod[["smooth"]][[2]], "random.effect")
})

testthat::test_that("fit_regression_model() builds the slope model", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      richness = stats::rnorm(n = 48),
      age = base::rep(base::seq(from = 0, to = 1, length.out = 12), times = 4),
      region = base::factor(base::rep(c("A", "B", "C", "D"), each = 12))
    )

  testthat::expect_warning(
    mod <-
      fit_regression_model(
        data_source = data_fit,
        y_var = "richness",
        time_var = "age",
        group_var = "region",
        random = "slope",
        sel_k = 4
      ),
    "repeated 1-d smooths"
  )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_equal(gratia::smooths(mod), c("s(age)", "s(age,region)"))
  testthat::expect_s3_class(mod[["smooth"]][[2]], "fs.interaction")
})

testthat::test_that("fit_regression_model() builds the slope_rand_id model", {
  base::set.seed(900723)

  data_fit <-
    tidyr::expand_grid(
      dataset_id = base::factor(base::seq_len(6)),
      age = base::seq(from = 0, to = 1, length.out = 12)
    ) |>
    dplyr::mutate(
      region = base::factor(
        dplyr::if_else(base::as.integer(dataset_id) <= 3L, "A", "B")
      ),
      richness = base::sin(3 * age) + stats::rnorm(n = dplyr::n(), sd = 0.2)
    )

  testthat::expect_warning(
    mod <-
      fit_regression_model(
        data_source = data_fit,
        y_var = "richness",
        time_var = "age",
        group_var = "region",
        random = "slope_rand_id",
        sel_k = 4
      ),
    "repeated 1-d smooths"
  )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_equal(
    gratia::smooths(mod),
    c("s(age)", "s(age,region)", "s(dataset_id)")
  )
  testthat::expect_equal(mod[["smooth"]][[1]][["bs.dim"]], 4L)
})

testthat::test_that("fit_regression_model() builds slope_rand_id_2 model", {
  base::set.seed(900723)

  data_fit <-
    tidyr::expand_grid(
      dataset_id = base::factor(base::seq_len(6)),
      age = base::seq(from = 0, to = 1, length.out = 12)
    ) |>
    dplyr::mutate(
      region = base::factor(
        dplyr::if_else(base::as.integer(dataset_id) <= 3L, "A", "B")
      ),
      richness = base::sin(3 * age) + stats::rnorm(n = dplyr::n(), sd = 0.2)
    )

  testthat::expect_warning(
    mod <-
      fit_regression_model(
        data_source = data_fit,
        y_var = "richness",
        time_var = "age",
        group_var = "region",
        random = "slope_rand_id_2",
        sel_k = 4
      ),
    "repeated 1-d smooths"
  )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_equal(
    gratia::smooths(mod),
    c("s(age)", "s(age,region)", "s(dataset_id)")
  )
  testthat::expect_equal(mod[["smooth"]][[1]][["bs.dim"]], 10L)
})

testthat::test_that("fit_regression_model() builds the intercept_reg model", {
  base::set.seed(900723)

  data_fit <-
    tidyr::expand_grid(
      dataset_id = base::factor(base::seq_len(6)),
      age = base::seq(from = 0, to = 1, length.out = 12)
    ) |>
    dplyr::mutate(
      region = base::factor(
        dplyr::if_else(base::as.integer(dataset_id) <= 3L, "A", "B")
      ),
      richness = base::sin(3 * age) + stats::rnorm(n = dplyr::n(), sd = 0.2)
    )

  mod <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept_reg",
      sel_k = 4
    )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_equal(
    gratia::smooths(mod),
    c("s(age):regionA", "s(age):regionB", "s(dataset_id)")
  )
  testthat::expect_true("regionB" %in% base::names(stats::coef(mod)))
  testthat::expect_s3_class(mod[["smooth"]][[1]], "tprs.smooth")
  testthat::expect_s3_class(mod[["smooth"]][[2]], "tprs.smooth")
  testthat::expect_equal(mod[["smooth"]][[1]][["bs.dim"]], 4L)

  # One smoothing parameter per group smooth, plus one for dataset_id
  testthat::expect_length(mod[["sp"]], 3L)
})

testthat::test_that("fit_regression_model() builds the hgam_GI model", {
  base::set.seed(900723)

  data_fit <-
    tidyr::expand_grid(
      dataset_id = base::factor(base::seq_len(6)),
      age = base::seq(from = 0, to = 1, length.out = 12)
    ) |>
    dplyr::mutate(
      region = base::factor(
        dplyr::if_else(base::as.integer(dataset_id) <= 3L, "A", "B")
      ),
      richness = base::sin(3 * age) + stats::rnorm(n = dplyr::n(), sd = 0.2)
    )

  mod <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "hgam_GI",
      sel_k = 4
    )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_equal(
    gratia::smooths(mod),
    c("s(age)", "s(age):regionA", "s(age):regionB", "s(dataset_id)")
  )
  testthat::expect_s3_class(mod[["smooth"]][[1]], "tprs.smooth")
  testthat::expect_equal(mod[["smooth"]][[1]][["bs.dim"]], 4L)

  # The global smooth and each group smooth get their own smoothing
  #   parameter, plus one for dataset_id
  testthat::expect_length(mod[["sp"]], 4L)
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("fit_regression_model() applies sel_k to the time smooth", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      richness = stats::rnorm(n = 48),
      age = base::rep(base::seq(from = 0, to = 1, length.out = 12), times = 4),
      region = base::factor(base::rep(c("A", "B", "C", "D"), each = 12))
    )

  mod_k4 <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4
    )

  mod_k8 <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 8
    )

  testthat::expect_equal(mod_k4[["smooth"]][[1]][["bs.dim"]], 4L)
  testthat::expect_equal(mod_k8[["smooth"]][[1]][["bs.dim"]], 8L)
})

testthat::test_that("fit_regression_model() uses the named variables", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      n_taxa = stats::rnorm(n = 48),
      time_bp = base::rep(base::seq(from = 0, to = 1, length.out = 12), 4),
      site = base::factor(base::rep(c("A", "B", "C", "D"), each = 12))
    )

  mod <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "n_taxa",
      time_var = "time_bp",
      group_var = "site",
      random = "intercept",
      sel_k = 4
    )

  testthat::expect_equal(gratia::smooths(mod), c("s(time_bp)", "s(site)"))
  testthat::expect_equal(
    base::as.character(stats::formula(mod)[[2]]),
    "n_taxa"
  )
})

testthat::test_that("fit_regression_model() recovers a smooth signal", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      age = base::rep(base::seq(from = 0, to = 1, length.out = 20), times = 4),
      region = base::factor(base::rep(c("A", "B", "C", "D"), each = 20))
    ) |>
    dplyr::mutate(
      signal = base::sin(2 * base::pi * age),
      richness = signal + stats::rnorm(n = dplyr::n(), sd = 0.1)
    )

  mod <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 8
    )

  testthat::expect_gt(
    stats::cor(
      x = base::as.vector(stats::fitted(mod)),
      y = dplyr::pull(data_fit, signal)
    ),
    0.9
  )
})

testthat::test_that("fit_regression_model() forwards error_family", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      age = base::rep(base::seq(from = 0, to = 1, length.out = 12), times = 4),
      region = base::factor(base::rep(c("A", "B", "C", "D"), each = 12))
    ) |>
    dplyr::mutate(
      richness = stats::rpois(n = dplyr::n(), lambda = 3 + 2 * age)
    )

  mod <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4,
      error_family = stats::poisson()
    )

  testthat::expect_equal(mod[["family"]][["family"]], "poisson")
  testthat::expect_true(base::all(stats::fitted(mod) > 0))
})

testthat::test_that("fit_regression_model() forwards arguments in ...", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      richness = stats::rnorm(n = 48),
      age = base::rep(base::seq(from = 0, to = 1, length.out = 12), times = 4),
      region = base::factor(base::rep(c("A", "B", "C", "D"), each = 12))
    )

  mod <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4,
      weights = base::rep(2, times = 48)
    )

  testthat::expect_equal(
    base::as.vector(mod[["prior.weights"]]),
    base::rep(2, times = 48)
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("fit_regression_model() drops rows with a missing y", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      richness = c(stats::rnorm(n = 47), NA_real_),
      age = base::rep(base::seq(from = 0, to = 1, length.out = 12), times = 4),
      region = base::factor(base::rep(c("A", "B", "C", "D"), each = 12))
    )

  mod <-
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4
    )

  testthat::expect_s3_class(mod, "bam")
  testthat::expect_equal(stats::nobs(mod), 47L)
})

testthat::test_that("fit_regression_model() errors for a single group", {
  base::set.seed(900723)

  data_fit <-
    tibble::tibble(
      richness = stats::rnorm(n = 12),
      age = base::seq(from = 0, to = 1, length.out = 12),
      region = base::factor(base::rep("A", times = 12))
    )

  testthat::expect_error(
    fit_regression_model(
      data_source = data_fit,
      y_var = "richness",
      time_var = "age",
      group_var = "region",
      random = "intercept",
      sel_k = 4
    ),
    "2 or more levels"
  )
})
