#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("bin_pollen_counts() accepts valid inputs", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a", "a", "b"),
      taxa = c("t1", "t1", "t2", "t1"),
      age = c(0, 0.4, 1.6, 1.9),
      pollen_counts = c(1, 2, 3, 4)
    )

  testthat::expect_no_error(
    res <-
      bin_pollen_counts(
        data_source = data_pollen,
        binning_var = dataset_id,
        bin_size = 1
      )
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::sum(dplyr::pull(res, summed_pollen_count)), 10)
})

testthat::test_that("bin_pollen_counts() validates data_source", {
  list_bad_data <-
    base::list(
      1,
      "a",
      base::list(age = 1),
      NULL
    )

  for (
    sel_data in list_bad_data
  ) {
    testthat::expect_error(
      bin_pollen_counts(
        data_source = sel_data,
        binning_var = dataset_id,
        bin_size = 1
      ),
      "data_source has to be a data.frame"
    )
  }
})

testthat::test_that("bin_pollen_counts() validates required columns", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a"),
      taxa = c("t1", "t2"),
      age = c(0, 1),
      pollen_counts = c(1, 2)
    )

  for (
    sel_col in c("age", "pollen_counts", "taxa")
  ) {
    data_missing <-
      dplyr::select(data_pollen, -dplyr::all_of(sel_col))

    testthat::expect_error(
      bin_pollen_counts(
        data_source = data_missing,
        binning_var = dataset_id,
        bin_size = 1
      ),
      "Missing required columns"
    )
  }
})

testthat::test_that("bin_pollen_counts() validates numeric columns", {
  data_text_age <-
    tibble::tibble(
      dataset_id = c("a", "a"),
      taxa = c("t1", "t2"),
      age = c("0", "1"),
      pollen_counts = c(1, 2)
    )

  data_text_counts <-
    dplyr::mutate(
      data_text_age,
      age = c(0, 1),
      pollen_counts = c("1", "2")
    )

  testthat::expect_error(
    bin_pollen_counts(
      data_source = data_text_age,
      binning_var = dataset_id,
      bin_size = 1
    ),
    "age and pollen_counts must be numeric"
  )

  testthat::expect_error(
    bin_pollen_counts(
      data_source = data_text_counts,
      binning_var = dataset_id,
      bin_size = 1
    ),
    "age and pollen_counts must be numeric"
  )
})

testthat::test_that("bin_pollen_counts() validates binning_var", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a"),
      taxa = c("t1", "t2"),
      age = c(0, 1),
      pollen_counts = c(1, 2)
    )

  testthat::expect_error(
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = not_a_column,
      bin_size = 1
    ),
    "binning_var column is missing in data_source"
  )

  testthat::expect_error(
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = "not_a_column",
      bin_size = 1
    ),
    "binning_var column is missing in data_source"
  )

  testthat::expect_error(
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = 1,
      bin_size = 1
    )
  )
})

testthat::test_that("bin_pollen_counts() validates bin_size", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a"),
      taxa = c("t1", "t2"),
      age = c(0, 1),
      pollen_counts = c(1, 2)
    )

  list_bad_size <-
    base::list(
      "a",
      c(1, 2),
      0,
      -1,
      NULL,
      numeric(0),
      NA_real_
    )

  for (
    sel_size in list_bad_size
  ) {
    testthat::expect_error(
      bin_pollen_counts(
        data_source = data_pollen,
        binning_var = dataset_id,
        bin_size = sel_size
      ),
      "bin_size must be a single positive numeric value"
    )
  }
})

testthat::test_that("bin_pollen_counts() rejects NA in age and pollen_counts", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a"),
      taxa = c("t1", "t2"),
      age = c(0, 1),
      pollen_counts = c(1, 2)
    )

  data_na_age <-
    dplyr::mutate(data_pollen, age = c(0, NA))

  data_na_counts <-
    dplyr::mutate(data_pollen, pollen_counts = c(1, NA))

  testthat::expect_error(
    bin_pollen_counts(
      data_source = data_na_age,
      binning_var = dataset_id,
      bin_size = 1
    ),
    "cannot contain NA"
  )

  testthat::expect_error(
    bin_pollen_counts(
      data_source = data_na_counts,
      binning_var = dataset_id,
      bin_size = 1
    ),
    "cannot contain NA"
  )
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("bin_pollen_counts() returns an ungrouped tibble", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a", "a", "b"),
      taxa = c("t1", "t1", "t2", "t1"),
      age = c(0, 0.4, 1.6, 1.9),
      pollen_counts = c(1, 2, 3, 4),
      latitude = c(10, 10, 10, 20)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 1
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_false(dplyr::is_grouped_df(res))
  testthat::expect_named(
    res,
    c("dataset_id", "taxa", "BIN", "summed_pollen_count")
  )
  testthat::expect_equal(base::dim(res), c(3L, 4L))
  testthat::expect_type(dplyr::pull(res, BIN), "integer")
  testthat::expect_equal(
    base::sort(base::unique(dplyr::pull(res, BIN))),
    c(1L, 2L)
  )
})

testthat::test_that("bin_pollen_counts() keeps the binning column name", {
  data_pollen <-
    tibble::tibble(
      core = c("a", "a", "b"),
      taxa = c("t1", "t1", "t1"),
      age = c(0, 0.4, 1.6),
      pollen_counts = c(1, 2, 3)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = core,
      bin_size = 1
    )

  testthat::expect_named(
    res,
    c("core", "taxa", "BIN", "summed_pollen_count")
  )
  testthat::expect_equal(base::nrow(res), 2L)
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("bin_pollen_counts() sums counts per group and bin", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a", "a", "b"),
      taxa = c("t1", "t1", "t2", "t1"),
      age = c(0, 0.4, 1.6, 1.9),
      pollen_counts = c(1, 2, 3, 4)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 1
    )

  data_expected <-
    tibble::tibble(
      dataset_id = c("a", "a", "b"),
      taxa = c("t1", "t2", "t1"),
      BIN = c(1L, 2L, 2L),
      summed_pollen_count = c(3, 3, 4)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("bin_pollen_counts() preserves total counts", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a", "a", "b", "b"),
      taxa = c("t1", "t1", "t2", "t1", "t2"),
      age = c(0, 0.4, 1.6, 1.9, 2.5),
      pollen_counts = c(1, 2, 3, 4, 5)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 1
    )

  testthat::expect_equal(
    base::sum(dplyr::pull(res, summed_pollen_count)),
    base::sum(dplyr::pull(data_pollen, pollen_counts))
  )

  data_taxa_res <-
    res |>
    dplyr::group_by(taxa) |>
    dplyr::summarise(
      total = base::sum(summed_pollen_count)
    )

  data_taxa_in <-
    data_pollen |>
    dplyr::group_by(taxa) |>
    dplyr::summarise(
      total = base::sum(pollen_counts)
    )

  testthat::expect_equal(data_taxa_res, data_taxa_in)
})

testthat::test_that("bin_pollen_counts() starts bins at the minimum age", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      taxa = c("t1", "t1"),
      age = c(0, 1.9),
      pollen_counts = c(1, 4)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 1
    )

  vec_bin_b <-
    res |>
    dplyr::filter(dataset_id == "b") |>
    dplyr::pull(BIN) |>
    base::as.character()

  testthat::expect_equal(vec_bin_b, "2")
})

testthat::test_that("bin_pollen_counts() bins negative ages from the minimum", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a", "a", "b"),
      taxa = c("t1", "t1", "t1", "t1"),
      age = c(-70, 929, 930, 2000),
      pollen_counts = c(1, 2, 4, 8)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 1000
    )

  data_expected <-
    tibble::tibble(
      dataset_id = c("a", "a", "b"),
      taxa = c("t1", "t1", "t1"),
      BIN = c(1L, 2L, 3L),
      summed_pollen_count = c(3, 4, 8)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("bin_pollen_counts() uses half-open bins [lower, upper)", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a", "a"),
      taxa = c("t1", "t1", "t1"),
      age = c(0, 0.99, 1),
      pollen_counts = c(1, 2, 4)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 1
    )

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_equal(dplyr::pull(res, summed_pollen_count), c(3, 4))
})

testthat::test_that("bin_pollen_counts() orders 12 bins chronologically", {
  data_pollen <-
    tibble::tibble(
      dataset_id = "a",
      taxa = "t1",
      age = c(1000 * (0:11), 1000 * (0:11) + 500),
      pollen_counts = c(10 * (1:12), base::rep(1, 12))
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 1000
    )

  data_res_by_bin <-
    tibble::tibble(
      bin = base::as.integer(base::as.character(dplyr::pull(res, BIN))),
      total = dplyr::pull(res, summed_pollen_count)
    ) |>
    dplyr::arrange(bin)

  testthat::expect_equal(dplyr::pull(data_res_by_bin, bin), 1:12)
  testthat::expect_equal(
    dplyr::pull(data_res_by_bin, total),
    10 * (1:12) + 1
  )
})

testthat::test_that("bin_pollen_counts() keeps the bin index across gaps", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a", "a"),
      taxa = c("t1", "t1", "t1"),
      age = c(0, 50, 3000),
      pollen_counts = c(1, 2, 3)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 1000
    )

  vec_bin <-
    res |>
    dplyr::pull(BIN) |>
    base::as.character() |>
    base::as.integer()

  testthat::expect_equal(vec_bin, c(1L, 4L))
})

testthat::test_that("bin_pollen_counts() accepts binning_var as a string", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "b"),
      taxa = c("t1", "t1"),
      age = c(0, 0.5),
      pollen_counts = c(1, 2)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = "dataset_id",
      bin_size = 1
    )

  testthat::expect_named(
    res,
    c("dataset_id", "taxa", "BIN", "summed_pollen_count")
  )
  testthat::expect_equal(dplyr::pull(res, dataset_id), c("a", "b"))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("bin_pollen_counts() handles a single row", {
  data_pollen <-
    tibble::tibble(
      dataset_id = "a",
      taxa = "t1",
      age = 100,
      pollen_counts = 7
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 500
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, summed_pollen_count), 7)
  testthat::expect_equal(base::as.character(dplyr::pull(res, BIN)), "1")
})

testthat::test_that("bin_pollen_counts() uses one bin for a large bin_size", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a", "b"),
      taxa = c("t1", "t1", "t1"),
      age = c(0, 4, 9),
      pollen_counts = c(1, 2, 3)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 100
    )

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_equal(dplyr::pull(res, BIN), c(1L, 1L))
  testthat::expect_equal(dplyr::pull(res, summed_pollen_count), c(3, 3))
})

testthat::test_that("bin_pollen_counts() sums duplicate rows and zero counts", {
  data_pollen <-
    tibble::tibble(
      dataset_id = c("a", "a", "a"),
      taxa = c("t1", "t1", "t1"),
      age = c(0, 0, 0.5),
      pollen_counts = c(5, 0, 2)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 1
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, summed_pollen_count), 7)
})

testthat::test_that("bin_pollen_counts() errors for zero rows", {
  data_pollen <-
    tibble::tibble(
      dataset_id = character(),
      taxa = character(),
      age = numeric(),
      pollen_counts = numeric()
    )

  testthat::expect_error(
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 1
    ),
    "at least one row"
  )
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("bin_pollen_counts() handles 5,000 rows", {
  base::set.seed(900723)

  data_pollen <-
    tibble::tibble(
      dataset_id = base::sample(base::paste0("d", 1:20), 5000, TRUE),
      taxa = base::sample(base::paste0("t", 1:5), 5000, TRUE),
      age = base::sample(0:200, 5000, TRUE),
      pollen_counts = base::sample(0:50, 5000, TRUE)
    )

  res <-
    bin_pollen_counts(
      data_source = data_pollen,
      binning_var = dataset_id,
      bin_size = 20
    )

  testthat::expect_named(
    res,
    c("dataset_id", "taxa", "BIN", "summed_pollen_count")
  )
  testthat::expect_true(base::nrow(res) <= 20 * 5 * 11)
  testthat::expect_equal(
    base::sum(dplyr::pull(res, summed_pollen_count)),
    base::sum(dplyr::pull(data_pollen, pollen_counts))
  )
  testthat::expect_equal(
    base::nrow(dplyr::distinct(res, dataset_id, taxa, BIN)),
    base::nrow(res)
  )
})
