#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("compute_richness() accepts valid inputs", {
  data_input <-
    tibble::tibble(
      dataset_id = c("d1", "d1"),
      age = c(100, 100),
      taxa = c("Betula", "Pinus"),
      pollen_grains = c(5, 3)
    )

  testthat::expect_no_error(
    res <-
      compute_richness(data_for_richness_estimation = data_input)
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(dplyr::pull(res, richness), 2)
})

testthat::test_that("compute_richness() rejects non-data-frame input", {
  list_bad_input <-
    base::list(
      NULL,
      1:3,
      "d1",
      base::list(dataset_id = "d1", age = 1, pollen_grains = 1)
    )

  for (
    sel_input in list_bad_input
  ) {
    testthat::expect_error(
      compute_richness(data_for_richness_estimation = sel_input),
      "data_for_richness_estimation has to be a data.frame"
    )
  }
})

testthat::test_that("compute_richness() validates required columns", {
  data_full <-
    tibble::tibble(
      dataset_id = "d1",
      age = 100,
      pollen_grains = 5
    )

  vec_required <-
    c("dataset_id", "age", "pollen_grains")

  for (
    sel_col in vec_required
  ) {
    data_missing <-
      dplyr::select(
        data_full,
        -dplyr::all_of(sel_col)
      )

    testthat::expect_error(
      compute_richness(data_for_richness_estimation = data_missing),
      "Input must contain columns"
    )
  }
})

testthat::test_that("compute_richness() requires numeric pollen_grains", {
  data_input <-
    tibble::tibble(
      dataset_id = "d1",
      age = 100,
      pollen_grains = "5"
    )

  testthat::expect_error(
    compute_richness(data_for_richness_estimation = data_input),
    "'pollen_grains' in data_for_richness_estimation must be numeric"
  )
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("compute_richness() returns an ungrouped tibble", {
  data_input <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2"),
      age = c(100, 200, 100),
      taxa = c("Betula", "Betula", "Pinus"),
      pollen_grains = c(5, 3, 2)
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_false(dplyr::is_grouped_df(res))
  testthat::expect_named(res, c("dataset_id", "age", "richness"))
  testthat::expect_equal(base::ncol(res), 3L)
  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_true(base::is.numeric(dplyr::pull(res, richness)))
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("compute_richness() counts present taxa per sample", {
  data_input <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d1", "d1", "d2"),
      age = c(100, 100, 100, 200, 200, 100),
      taxa = c("a", "b", "c", "a", "b", "a"),
      pollen_grains = c(5, 0.5, 1, 0, 0, 3)
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  data_expected <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2"),
      age = c(100, 200, 100),
      richness = c(2, 0, 1)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("compute_richness() uses a threshold of one grain", {
  data_input <-
    tibble::tibble(
      dataset_id = "d1",
      age = 100,
      taxa = c("a", "b", "c", "d"),
      pollen_grains = c(0.99, 1, 1.01, 100)
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  testthat::expect_equal(dplyr::pull(res, richness), 3)
})

testthat::test_that("compute_richness() does not count NA grains", {
  data_input <-
    tibble::tibble(
      dataset_id = "d1",
      age = c(100, 100, 200),
      taxa = c("a", "b", "a"),
      pollen_grains = c(NA, 4, NA_real_)
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  testthat::expect_equal(dplyr::pull(res, age), c(100, 200))
  testthat::expect_equal(dplyr::pull(res, richness), c(1, 0))
})

testthat::test_that("compute_richness() separates datasets and ages", {
  data_input <-
    tibble::tibble(
      dataset_id = c("d1", "d2", "d1", "d2"),
      age = c(100, 100, 200, 200),
      taxa = c("a", "a", "a", "a"),
      pollen_grains = c(1, 1, 1, 1)
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  testthat::expect_equal(base::nrow(res), 4L)
  testthat::expect_equal(dplyr::pull(res, richness), c(1, 1, 1, 1))
  testthat::expect_equal(
    base::nrow(dplyr::distinct(res, dataset_id, age)),
    base::nrow(res)
  )
})

testthat::test_that("compute_richness() is bounded by the rows per sample", {
  data_input <-
    tibble::tibble(
      dataset_id = base::rep(c("d1", "d2"), each = 6),
      age = base::rep(c(100, 200, 300), times = 4),
      taxa = base::rep(c("a", "b"), each = 3, times = 2),
      pollen_grains = c(0, 2, 5, 1, 0, 9, 3, 3, 3, 0, 0, 0)
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  data_n_rows <-
    dplyr::count(data_input, dataset_id, age)

  testthat::expect_true(
    base::all(dplyr::pull(res, richness) <= dplyr::pull(data_n_rows, n))
  )
  testthat::expect_true(base::all(dplyr::pull(res, richness) >= 0))
  testthat::expect_equal(
    base::sum(dplyr::pull(res, richness)),
    base::sum(dplyr::pull(data_input, pollen_grains) >= 1)
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("compute_richness() handles a single row", {
  data_input <-
    tibble::tibble(
      dataset_id = "d1",
      age = 100,
      pollen_grains = 1
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, richness), 1)
})

testthat::test_that("compute_richness() handles zero rows", {
  data_input <-
    tibble::tibble(
      dataset_id = character(),
      age = numeric(),
      pollen_grains = numeric()
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(res, c("dataset_id", "age", "richness"))
  testthat::expect_false(dplyr::is_grouped_df(res))
})

testthat::test_that("compute_richness() counts duplicated taxa twice", {
  data_input <-
    tibble::tibble(
      dataset_id = c("d1", "d1"),
      age = c(100, 100),
      taxa = c("a", "a"),
      pollen_grains = c(2, 2)
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  testthat::expect_equal(dplyr::pull(res, richness), 2)
})

testthat::test_that("compute_richness() ignores extra columns", {
  data_input <-
    tibble::tibble(
      dataset_id = "d1",
      age = 100,
      taxa = "a",
      pollen_grains = 3,
      region = "Europe"
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  testthat::expect_named(res, c("dataset_id", "age", "richness"))
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("compute_richness() scales to 10,000 rows", {
  data_input <-
    tidyr::expand_grid(
      dataset_id = base::paste0("d", base::seq_len(100)),
      age = base::seq_len(10) * 1000,
      taxa = base::paste0("t", base::seq_len(10))
    ) |>
    dplyr::mutate(
      pollen_grains = base::rep(base::seq_len(10) - 1, times = 1000)
    )

  res <-
    compute_richness(data_for_richness_estimation = data_input)

  testthat::expect_equal(base::nrow(data_input), 10000L)
  testthat::expect_equal(base::nrow(res), 1000L)
  testthat::expect_named(res, c("dataset_id", "age", "richness"))
  testthat::expect_true(base::all(dplyr::pull(res, richness) == 9))
})
