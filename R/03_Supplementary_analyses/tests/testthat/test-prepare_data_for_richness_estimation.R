#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_data_for_richness_estimation() accepts binned", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("d1", "d1"),
      BIN = c(1, 2),
      taxa = c("Betula", "Pinus"),
      summed_pollen_count = c(5, 3)
    )

  testthat::expect_no_error(
    res <-
      prepare_data_for_richness_estimation(
        data_source = data_binned,
        type = "binned"
      )
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 2L)
})

testthat::test_that("prepare_data_for_richness_estimation() accepts raw", {
  data_raw <-
    tibble::tibble(
      dataset_id = c("d1", "d1"),
      age = c(100, 200),
      taxa = c("Betula", "Pinus"),
      pollen_counts = c(5, 3)
    )

  testthat::expect_no_error(
    res <-
      prepare_data_for_richness_estimation(
        data_source = data_raw,
        type = "nonbinned"
      )
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 2L)
})

testthat::test_that("prepare_data_for_richness_estimation() checks data", {
  list_bad_input <-
    base::list(
      NULL,
      1:3,
      "d1",
      base::list(dataset_id = "d1")
    )

  for (
    sel_input in list_bad_input
  ) {
    testthat::expect_error(
      prepare_data_for_richness_estimation(
        data_source = sel_input,
        type = "binned"
      ),
      "data_source has to be a data.frame"
    )
  }
})

testthat::test_that("prepare_data_for_richness_estimation() checks type", {
  data_raw <-
    tibble::tibble(
      dataset_id = "d1",
      age = 100,
      taxa = "Betula",
      pollen_counts = 5
    )

  list_bad_type <-
    base::list(
      NULL,
      1,
      TRUE,
      c("binned", "nonbinned"),
      character(0),
      NA_character_,
      NA
    )

  for (
    sel_type in list_bad_type
  ) {
    testthat::expect_error(
      prepare_data_for_richness_estimation(
        data_source = data_raw,
        type = sel_type
      ),
      "type has to be a single non-NA character string"
    )
  }

  testthat::expect_error(
    prepare_data_for_richness_estimation(data_source = data_raw)
  )
})

testthat::test_that("prepare_data_for_richness_estimation() binned columns", {
  data_full <-
    tibble::tibble(
      dataset_id = "d1",
      BIN = 1,
      taxa = "Betula",
      summed_pollen_count = 5
    )

  vec_required <-
    c("dataset_id", "BIN", "taxa", "summed_pollen_count")

  for (
    sel_col in vec_required
  ) {
    data_missing <-
      dplyr::select(
        data_full,
        -dplyr::all_of(sel_col)
      )

    testthat::expect_error(
      prepare_data_for_richness_estimation(
        data_source = data_missing,
        type = "binned"
      ),
      "BIN and summed_pollen_count have to be columns"
    )
  }
})

testthat::test_that("prepare_data_for_richness_estimation() raw columns", {
  data_full <-
    tibble::tibble(
      dataset_id = "d1",
      age = 100,
      taxa = "Betula",
      pollen_counts = 5
    )

  vec_required <-
    c("dataset_id", "age", "taxa", "pollen_counts")

  for (
    sel_col in vec_required
  ) {
    data_missing <-
      dplyr::select(
        data_full,
        -dplyr::all_of(sel_col)
      )

    testthat::expect_error(
      prepare_data_for_richness_estimation(
        data_source = data_missing,
        type = "nonbinned"
      ),
      "age and pollen_counts have to be columns"
    )
  }
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("prepare_data_for_richness_estimation() output (binned)", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("d1", "d1"),
      BIN = c(1, 2),
      taxa = c("Betula", "Pinus"),
      summed_pollen_count = c(5, 3)
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_binned,
      type = "binned"
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("dataset_id", "age", "taxa", "pollen_grains")
  )
  testthat::expect_equal(base::ncol(res), 4L)
  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_type(dplyr::pull(res, age), "double")
})

testthat::test_that("prepare_data_for_richness_estimation() output (raw)", {
  data_raw <-
    tibble::tibble(
      dataset_id = c("d1", "d1"),
      age = c(100, 200),
      taxa = c("Betula", "Pinus"),
      pollen_counts = c(5, 3)
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_raw,
      type = "nonbinned"
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("dataset_id", "age", "taxa", "pollen_grains")
  )
  testthat::expect_equal(base::ncol(res), 4L)
  testthat::expect_equal(base::nrow(res), 2L)
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("prepare_data_for_richness_estimation() converts bins", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2"),
      BIN = c(1, 2, 5),
      taxa = c("Betula", "Pinus", "Betula"),
      summed_pollen_count = c(5, 3, 8)
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_binned,
      type = "binned"
    )

  data_expected <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2"),
      age = c(1000, 2000, 5000),
      taxa = c("Betula", "Pinus", "Betula"),
      pollen_grains = c(5, 3, 8)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("prepare_data_for_richness_estimation() bins from text", {
  data_binned <-
    tibble::tibble(
      dataset_id = c("d1", "d1"),
      BIN = c("3", "10"),
      taxa = c("Betula", "Pinus"),
      summed_pollen_count = c(5, 3)
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_binned,
      type = "binned"
    )

  testthat::expect_equal(dplyr::pull(res, age), c(3000, 10000))
})

testthat::test_that("prepare_data_for_richness_estimation() factor bins", {
  data_binned <-
    tibble::tibble(
      dataset_id = "d1",
      BIN = base::factor(c("1", "2", "10"), levels = c("1", "2", "10")),
      taxa = "Betula",
      summed_pollen_count = c(5, 3, 4)
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_binned,
      type = "binned"
    )

  testthat::expect_equal(dplyr::pull(res, age), c(1000, 2000, 10000))
})

testthat::test_that("prepare_data_for_richness_estimation() keeps raw ages", {
  data_raw <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2"),
      age = c(120, 4500, 10250),
      taxa = c("Betula", "Pinus", "Betula"),
      pollen_counts = c(5, 3, 8)
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_raw,
      type = "nonbinned"
    )

  data_expected <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2"),
      age = c(120, 4500, 10250),
      taxa = c("Betula", "Pinus", "Betula"),
      pollen_grains = c(5, 3, 8)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("prepare_data_for_richness_estimation() rejects bad type", {
  data_binned <-
    tibble::tibble(
      dataset_id = "d1",
      BIN = 1,
      taxa = "Betula",
      summed_pollen_count = 5
    )

  for (
    sel_type in c("anything_else", "Binned", "raw", "")
  ) {
    testthat::expect_error(
      prepare_data_for_richness_estimation(
        data_source = data_binned,
        type = sel_type
      ),
      "type has to be either 'binned' or 'nonbinned'"
    )
  }
})

testthat::test_that("prepare_data_for_richness_estimation() drops <= 0 (bin)", {
  data_binned <-
    tibble::tibble(
      dataset_id = "d1",
      BIN = c(1, 2, 3, 4, 5),
      taxa = c("a", "b", "c", "d", "e"),
      summed_pollen_count = c(0, -1, NA, 0.5, 7)
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_binned,
      type = "binned"
    )

  testthat::expect_equal(dplyr::pull(res, taxa), c("d", "e"))
  testthat::expect_equal(dplyr::pull(res, pollen_grains), c(0.5, 7))
  testthat::expect_equal(dplyr::pull(res, age), c(4000, 5000))
})

testthat::test_that("prepare_data_for_richness_estimation() drops <= 0 (raw)", {
  data_raw <-
    tibble::tibble(
      dataset_id = "d1",
      age = c(1, 2, 3, 4),
      taxa = c("a", "b", "c", "d"),
      pollen_counts = c(0, NA, 2, -3)
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_raw,
      type = "nonbinned"
    )

  testthat::expect_equal(dplyr::pull(res, taxa), "c")
  testthat::expect_true(base::all(dplyr::pull(res, pollen_grains) > 0))
})

testthat::test_that("prepare_data_for_richness_estimation() drops extras", {
  data_binned <-
    tibble::tibble(
      dataset_id = "d1",
      BIN = 1,
      taxa = "Betula",
      summed_pollen_count = 5,
      region = "Europe"
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_binned,
      type = "binned"
    )

  testthat::expect_named(
    res,
    c("dataset_id", "age", "taxa", "pollen_grains")
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("prepare_data_for_richness_estimation() zero rows", {
  data_binned <-
    tibble::tibble(
      dataset_id = character(),
      BIN = numeric(),
      taxa = character(),
      summed_pollen_count = numeric()
    )

  data_raw <-
    tibble::tibble(
      dataset_id = character(),
      age = numeric(),
      taxa = character(),
      pollen_counts = numeric()
    )

  res_binned <-
    prepare_data_for_richness_estimation(
      data_source = data_binned,
      type = "binned"
    )

  res_raw <-
    prepare_data_for_richness_estimation(
      data_source = data_raw,
      type = "nonbinned"
    )

  testthat::expect_equal(base::nrow(res_binned), 0L)
  testthat::expect_equal(base::nrow(res_raw), 0L)
  testthat::expect_named(
    res_binned,
    c("dataset_id", "age", "taxa", "pollen_grains")
  )
  testthat::expect_named(
    res_raw,
    c("dataset_id", "age", "taxa", "pollen_grains")
  )
})

testthat::test_that("prepare_data_for_richness_estimation() single row", {
  data_binned <-
    tibble::tibble(
      dataset_id = "d1",
      BIN = 7,
      taxa = "Betula",
      summed_pollen_count = 1
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_binned,
      type = "binned"
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, age), 7000)
})

testthat::test_that("prepare_data_for_richness_estimation() all dropped", {
  data_raw <-
    tibble::tibble(
      dataset_id = c("d1", "d1"),
      age = c(100, 200),
      taxa = c("Betula", "Pinus"),
      pollen_counts = c(0, 0)
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_raw,
      type = "nonbinned"
    )

  testthat::expect_equal(base::nrow(res), 0L)
})

testthat::test_that("prepare_data_for_richness_estimation() keeps dups", {
  data_raw <-
    tibble::tibble(
      dataset_id = c("d1", "d1"),
      age = c(100, 100),
      taxa = c("Betula", "Betula"),
      pollen_counts = c(5, 5)
    )

  res <-
    prepare_data_for_richness_estimation(
      data_source = data_raw,
      type = "nonbinned"
    )

  testthat::expect_equal(base::nrow(res), 2L)
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("prepare_data_for_richness_estimation() scales", {
  data_binned <-
    tidyr::expand_grid(
      dataset_id = base::paste0("d", base::seq_len(50)),
      BIN = base::seq_len(10),
      taxa = base::paste0("t", base::seq_len(10))
    ) |>
    dplyr::mutate(
      summed_pollen_count = base::rep(c(0, 1, 2, 3, 4), length.out = 5000)
    )

  res_binned <-
    prepare_data_for_richness_estimation(
      data_source = data_binned,
      type = "binned"
    )

  data_raw <-
    dplyr::rename(
      data_binned,
      age = BIN,
      pollen_counts = summed_pollen_count
    )

  res_raw <-
    prepare_data_for_richness_estimation(
      data_source = data_raw,
      type = "nonbinned"
    )

  testthat::expect_equal(base::nrow(data_binned), 5000L)
  testthat::expect_equal(base::nrow(res_binned), 4000L)
  testthat::expect_equal(base::nrow(res_raw), 4000L)
  testthat::expect_true(
    base::all(dplyr::pull(res_binned, age) %% 1000 == 0)
  )
  testthat::expect_true(base::all(dplyr::pull(res_binned, pollen_grains) > 0))
})
