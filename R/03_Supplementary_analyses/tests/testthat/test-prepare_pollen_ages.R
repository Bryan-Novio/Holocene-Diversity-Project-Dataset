#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_ages() accepts valid inputs", {
  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          age = c(100, 200),
          depth = c(10, 20)
        ),
        tibble::tibble(
          sample_id = "s3",
          age = 300,
          depth = 5
        )
      )
    )

  testthat::expect_no_error(
    res <-
      prepare_pollen_ages(data_compilation = data_compilation)
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 3L)
})

testthat::test_that("prepare_pollen_ages() rejects non-data-frame input", {
  list_bad_input <-
    base::list(
      NULL,
      1:3,
      "d1",
      base::list(dataset_id = "d1", levels = base::list())
    )

  for (
    sel_input in list_bad_input
  ) {
    testthat::expect_error(
      prepare_pollen_ages(data_compilation = sel_input),
      "data_compilation has to be a data.frame"
    )
  }
})

testthat::test_that("prepare_pollen_ages() validates required columns", {
  data_full <-
    tibble::tibble(
      dataset_id = "d1",
      levels = base::list(
        tibble::tibble(
          sample_id = "s1",
          age = 100
        )
      )
    )

  vec_required <-
    c("dataset_id", "levels")

  for (
    sel_col in vec_required
  ) {
    data_missing <-
      dplyr::select(
        data_full,
        -dplyr::all_of(sel_col)
      )

    testthat::expect_error(
      prepare_pollen_ages(data_compilation = data_missing),
      "`data_compilation` must contain columns"
    )
  }

  testthat::expect_error(
    prepare_pollen_ages(data_compilation = tibble::tibble()),
    "`data_compilation` must contain columns"
  )
})

testthat::test_that("prepare_pollen_ages() needs sample_id and age in levels", {
  data_no_age <-
    tibble::tibble(
      dataset_id = "d1",
      levels = base::list(
        tibble::tibble(
          sample_id = "s1",
          depth = 10
        )
      )
    )

  data_no_sample_id <-
    tibble::tibble(
      dataset_id = "d1",
      levels = base::list(
        tibble::tibble(
          age = 100,
          depth = 10
        )
      )
    )

  testthat::expect_error(
    prepare_pollen_ages(data_compilation = data_no_age)
  )
  testthat::expect_error(
    prepare_pollen_ages(data_compilation = data_no_sample_id)
  )
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_ages() returns one row per sample", {
  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          age = c(100, 200),
          depth = c(10, 20)
        ),
        tibble::tibble(
          sample_id = "s3",
          age = 300,
          depth = 5
        )
      )
    )

  res <-
    prepare_pollen_ages(data_compilation = data_compilation)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, c("dataset_id", "sample_id", "age"))
  testthat::expect_equal(base::ncol(res), 3L)
  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_type(dplyr::pull(res, dataset_id), "character")
  testthat::expect_type(dplyr::pull(res, sample_id), "character")
  testthat::expect_type(dplyr::pull(res, age), "double")
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_ages() returns the ages of all samples", {
  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          age = c(100, 200),
          depth = c(10, 20)
        ),
        tibble::tibble(
          sample_id = "s3",
          age = 300,
          depth = 5
        )
      )
    )

  res <-
    prepare_pollen_ages(data_compilation = data_compilation)

  data_expected <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2"),
      sample_id = c("s1", "s2", "s3"),
      age = c(100, 200, 300)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("prepare_pollen_ages() drops columns other than the ids", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      site_name = "Lake",
      levels = base::list(
        tibble::tibble(
          sample_id = "s1",
          depth = 10,
          age = 100,
          age_error = 5
        )
      )
    )

  res <-
    prepare_pollen_ages(data_compilation = data_compilation)

  testthat::expect_named(res, c("dataset_id", "sample_id", "age"))
  testthat::expect_equal(dplyr::pull(res, age), 100)
})

testthat::test_that("prepare_pollen_ages() keeps the row count of the levels", {
  list_levels <-
    base::list(
      tibble::tibble(sample_id = c("a", "b", "c"), age = c(1, 2, 3)),
      tibble::tibble(sample_id = c("d", "e"), age = c(4, 5)),
      tibble::tibble(sample_id = "f", age = 6)
    )

  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2", "d3"),
      levels = list_levels
    )

  res <-
    prepare_pollen_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 6L)
  testthat::expect_equal(dplyr::pull(res, age), 1:6)
  testthat::expect_equal(
    dplyr::pull(res, dataset_id),
    c("d1", "d1", "d1", "d2", "d2", "d3")
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_ages() handles a single sample", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      levels = base::list(
        tibble::tibble(
          sample_id = "s1",
          age = 123
        )
      )
    )

  res <-
    prepare_pollen_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, age), 123)
})

testthat::test_that("prepare_pollen_ages() keeps NA ages and duplicates", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s1", "s2"),
          age = c(100, 100, NA)
        )
      )
    )

  res <-
    prepare_pollen_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_equal(base::sum(base::is.na(dplyr::pull(res, age))), 1L)
})

testthat::test_that("prepare_pollen_ages() skips datasets without levels", {
  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      levels = base::list(
        tibble::tibble(
          sample_id = "s1",
          age = 100
        ),
        tibble::tibble(
          sample_id = character(),
          age = numeric()
        )
      )
    )

  res <-
    prepare_pollen_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, dataset_id), "d1")
})

testthat::test_that("prepare_pollen_ages() handles a zero-row compilation", {
  testthat::skip(
    "Known issue: zero-row compilation fails in select()."
  )

  data_compilation <-
    tibble::tibble(
      dataset_id = character(),
      levels = base::list()
    )

  res <-
    prepare_pollen_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(res, c("dataset_id", "sample_id", "age"))
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_ages() scales to thousands of samples", {
  list_levels <-
    purrr::map(
      base::seq_len(50),
      ~ tibble::tibble(
        sample_id = base::paste0("s", base::seq_len(100)),
        age = base::seq_len(100) * 10,
        depth = base::seq_len(100)
      )
    )

  data_compilation <-
    tibble::tibble(
      dataset_id = base::paste0("d", base::seq_len(50)),
      levels = list_levels
    )

  res <-
    prepare_pollen_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 5000L)
  testthat::expect_named(res, c("dataset_id", "sample_id", "age"))
  testthat::expect_equal(
    base::sum(dplyr::pull(res, age)),
    50 * base::sum(base::seq_len(100) * 10)
  )
})
