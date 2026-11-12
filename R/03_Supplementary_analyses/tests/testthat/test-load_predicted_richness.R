#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("load_predicted_richness() accepts valid inputs", {
  dir_data <-
    withr::local_tempdir()

  data_iter <-
    tibble::tibble(
      region = c("Europe", "Asia"),
      age = c(100, 200),
      richness = c(10, 20)
    )

  base::saveRDS(
    object = data_iter,
    file = base::file.path(dir_data, "1.rds")
  )

  testthat::expect_no_error(
    res <-
      load_predicted_richness(
        iteration = 1,
        path = dir_data
      )
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 2L)
})

testthat::test_that("load_predicted_richness() validates iteration", {
  dir_data <-
    withr::local_tempdir()

  list_bad_iteration <-
    base::list(
      c(1, 2),
      NA,
      NA_real_,
      NA_character_,
      TRUE,
      base::list(1),
      NULL,
      numeric(0)
    )

  for (
    sel_bad in list_bad_iteration
  ) {
    testthat::expect_error(
      load_predicted_richness(
        iteration = sel_bad,
        path = dir_data
      ),
      "`iteration` must be a single number or string\\."
    )
  }
})

testthat::test_that("load_predicted_richness() validates path", {
  list_bad_path <-
    base::list(
      1,
      c("a", "b"),
      NA_character_,
      NULL,
      character(0),
      TRUE
    )

  for (
    sel_bad in list_bad_path
  ) {
    testthat::expect_error(
      load_predicted_richness(
        iteration = 1,
        path = sel_bad
      ),
      "`path` must be a single character string\\."
    )
  }
})

testthat::test_that("load_predicted_richness() checks file exists", {
  dir_data <-
    withr::local_tempdir()

  testthat::expect_error(
    load_predicted_richness(
      iteration = 1,
      path = dir_data
    ),
    "does not exist"
  )

  testthat::expect_error(
    load_predicted_richness(
      iteration = 1,
      path = base::file.path(dir_data, "no_such_dir")
    ),
    "does not exist"
  )
})

testthat::test_that("load_predicted_richness() validates columns", {
  dir_data <-
    withr::local_tempdir()

  data_full <-
    tibble::tibble(
      region = c("Europe", "Asia"),
      age = c(100, 200),
      richness = c(10, 20)
    )

  vec_required <-
    c("region", "age", "richness")

  for (
    sel_col in vec_required
  ) {
    data_missing <-
      dplyr::select(
        data_full,
        -dplyr::all_of(sel_col)
      )

    base::saveRDS(
      object = data_missing,
      file = base::file.path(dir_data, "1.rds")
    )

    testthat::expect_error(
      load_predicted_richness(
        iteration = 1,
        path = dir_data
      ),
      "must hold a data frame with 'region', 'age', 'richness'"
    )
  }
})

testthat::test_that("load_predicted_richness() rejects non-data files", {
  dir_data <-
    withr::local_tempdir()

  base::saveRDS(
    object = base::list(region = "Europe", age = 100, richness = 10),
    file = base::file.path(dir_data, "1.rds")
  )

  testthat::expect_error(
    load_predicted_richness(
      iteration = 1,
      path = dir_data
    ),
    "must hold a data frame"
  )
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("load_predicted_richness() keeps three columns", {
  dir_data <-
    withr::local_tempdir()

  data_iter <-
    tibble::tibble(
      rich_high = c(12, 24),
      richness = c(10, 20),
      region = c("Europe", "Asia"),
      rich_low = c(8, 16),
      age = c(100, 200)
    )

  base::saveRDS(
    object = data_iter,
    file = base::file.path(dir_data, "1.rds")
  )

  res <-
    load_predicted_richness(
      iteration = 1,
      path = dir_data
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, c("region", "age", "richness"))
  testthat::expect_equal(base::ncol(res), 3L)
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("load_predicted_richness() returns file values", {
  dir_data <-
    withr::local_tempdir()

  data_iter <-
    tibble::tibble(
      region = c("Europe", "Asia", "Europe"),
      age = c(100, 200, 300),
      richness = c(10.5, 20.25, 30),
      rich_low = c(8, 16, 24)
    )

  base::saveRDS(
    object = data_iter,
    file = base::file.path(dir_data, "1.rds")
  )

  res <-
    load_predicted_richness(
      iteration = 1,
      path = dir_data
    )

  data_expected <-
    tibble::tibble(
      region = c("Europe", "Asia", "Europe"),
      age = c(100, 200, 300),
      richness = c(10.5, 20.25, 30)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("load_predicted_richness() reads the iteration", {
  dir_data <-
    withr::local_tempdir()

  base::saveRDS(
    object = tibble::tibble(region = "A", age = 1, richness = 1),
    file = base::file.path(dir_data, "1.rds")
  )

  base::saveRDS(
    object = tibble::tibble(region = "B", age = 2, richness = 2),
    file = base::file.path(dir_data, "2.rds")
  )

  res_1 <-
    load_predicted_richness(
      iteration = 1,
      path = dir_data
    )

  res_2 <-
    load_predicted_richness(
      iteration = 2,
      path = dir_data
    )

  testthat::expect_equal(dplyr::pull(res_1, region), "A")
  testthat::expect_equal(dplyr::pull(res_2, region), "B")
  testthat::expect_equal(dplyr::pull(res_2, richness), 2)
})

testthat::test_that("load_predicted_richness() takes number or string", {
  dir_data <-
    withr::local_tempdir()

  base::saveRDS(
    object = tibble::tibble(region = "A", age = 1, richness = 1),
    file = base::file.path(dir_data, "7.rds")
  )

  res_number <-
    load_predicted_richness(
      iteration = 7,
      path = dir_data
    )

  res_string <-
    load_predicted_richness(
      iteration = "7",
      path = dir_data
    )

  testthat::expect_equal(res_number, res_string)
  testthat::expect_equal(base::nrow(res_number), 1L)
})

testthat::test_that("load_predicted_richness() works when mapped", {
  dir_data <-
    withr::local_tempdir()

  for (
    sel_iter in 1:3
  ) {
    base::saveRDS(
      object = tibble::tibble(
        region = "A",
        age = 1,
        richness = sel_iter * 10
      ),
      file = base::file.path(dir_data, base::paste0(sel_iter, ".rds"))
    )
  }

  data_all <-
    purrr::map(
      .x = base::seq_len(3),
      .f = load_predicted_richness,
      path = dir_data
    ) |>
    dplyr::bind_rows()

  testthat::expect_equal(dplyr::pull(data_all, richness), c(10, 20, 30))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("load_predicted_richness() handles zero rows", {
  dir_data <-
    withr::local_tempdir()

  data_empty <-
    tibble::tibble(
      region = character(0),
      age = numeric(0),
      richness = numeric(0)
    )

  base::saveRDS(
    object = data_empty,
    file = base::file.path(dir_data, "1.rds")
  )

  res <-
    load_predicted_richness(
      iteration = 1,
      path = dir_data
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(res, c("region", "age", "richness"))
})

testthat::test_that("load_predicted_richness() accepts trailing slash", {
  dir_data <-
    withr::local_tempdir()

  base::saveRDS(
    object = tibble::tibble(region = "A", age = 1, richness = 1),
    file = base::file.path(dir_data, "1.rds")
  )

  res <-
    load_predicted_richness(
      iteration = 1,
      path = base::paste0(dir_data, "/")
    )

  testthat::expect_equal(base::nrow(res), 1L)
})

testthat::test_that("load_predicted_richness() has a project default", {
  expr_default <-
    base::formals(load_predicted_richness)[["path"]]

  testthat::expect_equal(
    base::eval(expr_default),
    here::here("Data/Paper_1/data_model/data_back_new/")
  )
})
