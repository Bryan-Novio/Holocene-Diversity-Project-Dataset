#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("filter_datasets_by_n_bins() valid", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "a", "b", "b", "c"),
      taxa = c("t1", "t1", "t2", "t1", "t2", "t1"),
      BIN = c(1L, 2L, 3L, 1L, 1L, 5L)
    )

  testthat::expect_no_error(
    res <-
      filter_datasets_by_n_bins(
        data_input = data_binned,
        n_bins = 3
      )
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 3L)
})

testthat::test_that("filter_datasets_by_n_bins() data_input", {
  list_bad_data <-
    base::list(
      1,
      "a",
      base::list(dataset_id = "a", BIN = 1L),
      NULL
    )

  for (
    sel_data in list_bad_data
  ) {
    testthat::expect_error(
      filter_datasets_by_n_bins(
        data_input = sel_data,
        n_bins = 1
      ),
      "data_input has to be data.frame"
    )
  }
})

testthat::test_that("filter_datasets_by_n_bins() cols", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      BIN = c(1L, 2L)
    )

  for (
    sel_col in c("dataset_id", "BIN")
  ) {
    data_missing <-
      dplyr::select(data_binned, -dplyr::all_of(sel_col))

    testthat::expect_error(
      filter_datasets_by_n_bins(
        data_input = data_missing,
        n_bins = 1
      ),
      "must contain 'dataset_id' and 'BIN'"
    )
  }
})

testthat::test_that("filter_datasets_by_n_bins() n_bins", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      BIN = c(1L, 2L)
    )

  list_bad_n <-
    base::list(
      "a",
      c(1, 2),
      NA_real_,
      NULL,
      numeric(0),
      TRUE
    )

  for (
    sel_n in list_bad_n
  ) {
    testthat::expect_error(
      filter_datasets_by_n_bins(
        data_input = data_binned,
        n_bins = sel_n
      ),
      "n_bins must be a single numeric, non-NA value"
    )
  }
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("filter_datasets_by_n_bins() shape", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "a", "b", "b", "c"),
      taxa = c("t1", "t1", "t2", "t1", "t2", "t1"),
      BIN = c(1L, 2L, 3L, 1L, 1L, 5L)
    )

  res <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 3
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_false(dplyr::is_grouped_df(res))
  testthat::expect_named(res, c("dataset_id", "taxa", "BIN"))
  testthat::expect_equal(base::dim(res), c(3L, 3L))
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("filter_datasets_by_n_bins() keeps rows", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "a", "b", "b", "c"),
      taxa = c("t1", "t1", "t2", "t1", "t2", "t1"),
      BIN = c(1L, 2L, 3L, 1L, 1L, 5L)
    )

  res <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 2
    )

  data_expected <-
    tibble::tibble(
      dataset_id = c("a", "a", "a"),
      taxa = c("t1", "t1", "t2"),
      BIN = c(1L, 2L, 3L)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("filter_datasets_by_n_bins() boundary", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "a", "b", "b", "c"),
      BIN = c(1L, 2L, 3L, 1L, 2L, 5L)
    )

  res_equal <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 3
    )

  res_above <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 4
    )

  res_two <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 2
    )

  testthat::expect_equal(base::unique(dplyr::pull(res_equal, dataset_id)), "a")
  testthat::expect_equal(base::nrow(res_above), 0L)
  testthat::expect_equal(
    base::unique(dplyr::pull(res_two, dataset_id)),
    c("a", "b")
  )
})

testthat::test_that("filter_datasets_by_n_bins() distinct", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "a", "a"),
      taxa = c("t1", "t2", "t3", "t4"),
      BIN = c(1L, 1L, 1L, 1L)
    )

  res_two <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 2
    )

  res_one <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 1
    )

  testthat::expect_equal(base::nrow(res_two), 0L)
  testthat::expect_equal(base::nrow(res_one), 4L)
})

testthat::test_that("filter_datasets_by_n_bins() order", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("b", "a", "b", "a"),
      BIN = c(2L, 1L, 1L, 2L)
    )

  res <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 2
    )

  testthat::expect_equal(res, data_binned)
})

testthat::test_that("filter_datasets_by_n_bins() factor BIN", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "b"),
      BIN = base::factor(c("1", "2", "1"))
    )

  res <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 2
    )

  testthat::expect_equal(dplyr::pull(res, dataset_id), c("a", "a"))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("filter_datasets_by_n_bins() low n_bins", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      BIN = c(1L, 2L)
    )

  res_zero <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 0
    )

  res_negative <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = -3
    )

  testthat::expect_equal(res_zero, data_binned)
  testthat::expect_equal(res_negative, data_binned)
})

testthat::test_that("filter_datasets_by_n_bins() 1 row", {
  data_binned <-
    tibble::tibble(
      dataset_id = "a",
      BIN = 1L
    )

  res_keep <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 1
    )

  res_drop <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 2
    )

  testthat::expect_equal(base::nrow(res_keep), 1L)
  testthat::expect_equal(base::nrow(res_drop), 0L)
})

testthat::test_that("filter_datasets_by_n_bins() 0 rows", {
  data_binned <-
    tibble::tibble(
      dataset_id = character(),
      BIN = integer()
    )

  res <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 1
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(res, c("dataset_id", "BIN"))
})

testthat::test_that("filter_datasets_by_n_bins() grouped", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "b"),
      BIN = c(1L, 2L, 1L)
    ) |>
    dplyr::group_by(dataset_id)

  res <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 2
    )

  testthat::expect_equal(dplyr::pull(res, dataset_id), c("a", "a"))
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("filter_datasets_by_n_bins() large", {
  base::set.seed(900723)

  data_binned <-
    tibble::tibble(
      dataset_id = base::sample(base::paste0("d", 1:200), 5000, TRUE),
      taxa = base::sample(base::paste0("t", 1:5), 5000, TRUE),
      BIN = base::sample(1:30, 5000, TRUE)
    )

  res <-
    filter_datasets_by_n_bins(
      data_input = data_binned,
      n_bins = 20
    )

  data_n_bins <-
    data_binned |>
    dplyr::distinct(dataset_id, BIN) |>
    dplyr::count(dataset_id)

  vec_expected_ids <-
    data_n_bins |>
    dplyr::filter(n >= 20) |>
    dplyr::pull(dataset_id)

  testthat::expect_named(res, c("dataset_id", "taxa", "BIN"))
  testthat::expect_equal(
    base::sort(base::unique(dplyr::pull(res, dataset_id))),
    base::sort(vec_expected_ids)
  )
  testthat::expect_equal(
    base::nrow(res),
    base::sum(dplyr::pull(data_binned, dataset_id) %in% vec_expected_ids)
  )
})
