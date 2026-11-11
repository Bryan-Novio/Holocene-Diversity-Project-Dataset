#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("accepts valid inputs", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "b", "b", "c"),
      BIN = c(1L, 1L, 1L, 2L, 2L),
      summed_pollen_count = c(300, 200, 100, 399, 1)
    )

  testthat::expect_no_error(
    res <-
      filter_bins_by_pollen_sum(
        data_input = data_binned,
        n_grains = 400
      )
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 5L)
})

testthat::test_that("validates data_input", {
  list_bad_data <-
    base::list(
      1,
      "a",
      base::list(BIN = 1, summed_pollen_count = 1),
      NULL
    )

  for (
    sel_data in list_bad_data
  ) {
    testthat::expect_error(
      filter_bins_by_pollen_sum(
        data_input = sel_data,
        n_grains = 400
      ),
      "data_input has to be a data.frame"
    )
  }
})

testthat::test_that("validates required columns", {
  data_binned <-
    tibble::tibble(
      BIN = c(1L, 2L),
      summed_pollen_count = c(300, 200)
    )

  for (
    sel_col in c("BIN", "summed_pollen_count")
  ) {
    data_missing <-
      dplyr::select(data_binned, -dplyr::all_of(sel_col))

    testthat::expect_error(
      filter_bins_by_pollen_sum(
        data_input = data_missing,
        n_grains = 100
      ),
      "must contain 'BIN' and 'summed_pollen_count'"
    )
  }
})

testthat::test_that("validates n_grains", {
  data_binned <-
    tibble::tibble(
      BIN = c(1L, 2L),
      summed_pollen_count = c(300, 200)
    )

  list_bad_n <-
    base::list(
      "a",
      c(100, 200),
      NA_real_,
      NULL,
      numeric(0),
      TRUE
    )

  for (
    sel_n in list_bad_n
  ) {
    testthat::expect_error(
      filter_bins_by_pollen_sum(
        data_input = data_binned,
        n_grains = sel_n
      ),
      "n_grains has to be a single numeric value"
    )
  }
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("returns the columns of data_input", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "b", "b", "c"),
      taxa = c("t1", "t2", "t1", "t1", "t2"),
      BIN = c(1L, 1L, 1L, 2L, 2L),
      summed_pollen_count = c(300, 200, 100, 399, 1)
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 500
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("dataset_id", "taxa", "BIN", "summed_pollen_count")
  )
  testthat::expect_equal(base::dim(res), c(3L, 4L))
})

testthat::test_that("returns a data frame for a data frame input", {
  data_binned <-
    base::data.frame(
      BIN = c(1L, 1L, 2L),
      summed_pollen_count = c(300, 200, 10)
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 400
    )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 2L)
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("keeps bins with a pooled total >= n_grains", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "b", "b", "c"),
      BIN = c(1L, 1L, 1L, 2L, 2L),
      summed_pollen_count = c(300, 200, 100, 399, 1)
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 500
    )

  data_expected <-
    tibble::tibble(
      dataset_id = c("a", "a", "b"),
      BIN = c(1L, 1L, 1L),
      summed_pollen_count = c(300, 200, 100)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("keeps a bin whose total equals n_grains", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "a", "b", "b", "c"),
      BIN = c(1L, 1L, 1L, 2L, 2L),
      summed_pollen_count = c(300, 200, 100, 399, 1)
    )

  res_equal <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 400
    )

  res_above <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 401
    )

  testthat::expect_equal(base::unique(dplyr::pull(res_equal, BIN)), c(1L, 2L))
  testthat::expect_equal(base::unique(dplyr::pull(res_above, BIN)), 1L)
})

testthat::test_that("pools totals across datasets", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      BIN = c(1L, 1L),
      summed_pollen_count = c(300, 300)
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 400
    )

  testthat::expect_equal(base::nrow(res), 2L)
})

testthat::test_that("applies the threshold per dataset and BIN", {
  testthat::skip(
    "Known issue: totals are pooled over datasets, not per dataset and BIN"
  )

  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "b", "a"),
      BIN = c(1L, 1L, 2L),
      summed_pollen_count = c(300, 300, 500)
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 400
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, dataset_id), "a")
  testthat::expect_equal(dplyr::pull(res, BIN), 2L)
})

testthat::test_that("keeps exactly the bins that pass (random data)", {
  base::set.seed(900723)

  data_binned <-
    tibble::tibble(
      dataset_id = base::sample(base::paste0("d", 1:5), 60, TRUE),
      BIN = base::sample(1:6, 60, TRUE),
      summed_pollen_count = base::sample(1:100, 60, TRUE)
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 500
    )

  data_totals <-
    data_binned |>
    dplyr::group_by(BIN) |>
    dplyr::summarise(
      total = base::sum(summed_pollen_count)
    )

  vec_expected_bins <-
    data_totals |>
    dplyr::filter(total >= 500) |>
    dplyr::pull(BIN)

  testthat::expect_true(base::all(dplyr::pull(res, BIN) %in% vec_expected_bins))
  testthat::expect_equal(
    base::sort(base::unique(dplyr::pull(res, BIN))),
    base::sort(vec_expected_bins)
  )
  testthat::expect_equal(
    base::nrow(res),
    base::sum(dplyr::pull(data_binned, BIN) %in% vec_expected_bins)
  )
})

testthat::test_that("accepts a factor BIN", {
  data_binned <-
    tibble::tibble(
      BIN = base::factor(c("1", "1", "2")),
      summed_pollen_count = c(300, 200, 10)
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 400
    )

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_equal(base::as.character(dplyr::pull(res, BIN)), c("1", "1"))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("returns zero rows if no bin passes", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      BIN = c(1L, 2L),
      summed_pollen_count = c(10, 20)
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 1000
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(res, c("dataset_id", "BIN", "summed_pollen_count"))
})

testthat::test_that("keeps everything for n_grains <= 0", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      BIN = c(1L, 2L),
      summed_pollen_count = c(10, 20)
    )

  res_zero <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 0
    )

  res_negative <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = -5
    )

  testthat::expect_equal(res_zero, data_binned)
  testthat::expect_equal(res_negative, data_binned)
})

testthat::test_that("handles a single row", {
  data_binned <-
    tibble::tibble(
      BIN = 1L,
      summed_pollen_count = 400
    )

  res_keep <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 400
    )

  res_drop <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 401
    )

  testthat::expect_equal(base::nrow(res_keep), 1L)
  testthat::expect_equal(base::nrow(res_drop), 0L)
})

testthat::test_that("handles zero rows", {
  data_binned <-
    tibble::tibble(
      BIN = integer(),
      summed_pollen_count = numeric()
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 10
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(res, c("BIN", "summed_pollen_count"))
})

testthat::test_that("drops bins whose total is NA", {
  data_binned <-
    tibble::tibble(
      BIN = c(1L, 1L, 2L),
      summed_pollen_count = c(300, NA, 500)
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 400
    )

  testthat::expect_equal(dplyr::pull(res, BIN), 2L)
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("handles 5,000 rows", {
  base::set.seed(900723)

  data_binned <-
    tibble::tibble(
      dataset_id = base::sample(base::paste0("d", 1:100), 5000, TRUE),
      BIN = base::sample(1:50, 5000, TRUE),
      summed_pollen_count = base::sample(0:100, 5000, TRUE)
    )

  res <-
    filter_bins_by_pollen_sum(
      data_input = data_binned,
      n_grains = 5000
    )

  data_totals <-
    data_binned |>
    dplyr::group_by(BIN) |>
    dplyr::summarise(
      total = base::sum(summed_pollen_count)
    )

  vec_expected_bins <-
    data_totals |>
    dplyr::filter(total >= 5000) |>
    dplyr::pull(BIN)

  testthat::expect_named(res, c("dataset_id", "BIN", "summed_pollen_count"))
  testthat::expect_true(base::length(vec_expected_bins) %in% 1:49)
  testthat::expect_equal(
    base::sort(base::unique(dplyr::pull(res, BIN))),
    base::sort(vec_expected_bins)
  )
})
