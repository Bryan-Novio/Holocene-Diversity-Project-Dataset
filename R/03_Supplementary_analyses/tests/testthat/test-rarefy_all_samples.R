#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("rarefy_all_samples() accepts valid inputs", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  testthat::expect_no_error(
    res <-
      rarefy_all_samples(
        data_source = data_wide,
        n_grains = 10
      )
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 3L)
})

testthat::test_that("rarefy_all_samples() validates data_source", {
  list_bad_data <-
    base::list(
      1,
      "a",
      base::list(dataset_id = "a", age = 1),
      NULL
    )

  for (
    sel_data in list_bad_data
  ) {
    testthat::expect_error(
      rarefy_all_samples(
        data_source = sel_data,
        n_grains = 10
      ),
      "data_source has to be data.frame"
    )
  }
})

testthat::test_that("rarefy_all_samples() validates required columns", {
  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 200),
      taxon_a = c(5L, 3L)
    )

  for (
    sel_col in c("dataset_id", "age")
  ) {
    data_missing <-
      dplyr::select(data_wide, -dplyr::all_of(sel_col))

    testthat::expect_error(
      rarefy_all_samples(
        data_source = data_missing,
        n_grains = 3
      ),
      "must contain columns 'dataset_id', 'age'"
    )
  }
})

testthat::test_that("rarefy_all_samples() validates n_grains", {
  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 200),
      taxon_a = c(5L, 3L),
      taxon_b = c(1L, 2L)
    )

  list_bad_n <-
    base::list(
      "a",
      c(3, 4),
      0,
      -5,
      2.5,
      NA_real_,
      Inf,
      NULL,
      numeric(0),
      TRUE
    )

  for (
    sel_n in list_bad_n
  ) {
    testthat::expect_error(
      rarefy_all_samples(
        data_source = data_wide,
        n_grains = sel_n
      ),
      "n_grains must be a single positive whole number"
    )
  }
})

testthat::test_that("rarefy_all_samples() requires rows and taxon columns", {
  data_no_rows <-
    tibble::tibble(
      dataset_id = character(),
      age = numeric(),
      taxon_a = integer(),
      taxon_b = integer()
    )

  data_no_taxa <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 200)
    )

  data_one_taxon <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 200),
      taxon_a = c(8L, 1L)
    )

  testthat::expect_error(
    rarefy_all_samples(
      data_source = data_no_rows,
      n_grains = 10
    ),
    "at least one row"
  )

  testthat::expect_error(
    rarefy_all_samples(
      data_source = data_no_taxa,
      n_grains = 10
    ),
    "at least two taxon columns"
  )

  testthat::expect_error(
    rarefy_all_samples(
      data_source = data_one_taxon,
      n_grains = 1
    ),
    "at least two taxon columns"
  )
})

testthat::test_that("rarefy_all_samples() rejects non-integer counts", {
  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 200),
      taxon_a = c(5.5, 3),
      taxon_b = c(1, 2)
    )

  testthat::expect_error(
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 3
    ),
    "integers"
  )
})

testthat::test_that("rarefy_all_samples() rejects duplicated sample ids", {
  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001"),
      age = c(100, 100),
      taxon_a = c(5L, 3L),
      taxon_b = c(1L, 2L)
    )

  testthat::expect_error(
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 3
    ),
    "dataset_id and age must be unique"
  )
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("rarefy_all_samples() returns sample ids and taxa", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  res <-
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 10
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("dataset_id_age", "taxon_a", "taxon_b", "taxon_c")
  )
  testthat::expect_equal(base::dim(res), c(3L, 4L))
  testthat::expect_type(dplyr::pull(res, dataset_id_age), "character")
  testthat::expect_true(base::is.numeric(dplyr::pull(res, taxon_a)))
})

testthat::test_that("rarefy_all_samples() builds dataset_id_age ids", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200.5, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  res <-
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 10
    )

  testthat::expect_equal(
    dplyr::pull(res, dataset_id_age),
    c("1001_100", "1001_200.5", "1002_100")
  )
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("rarefy_all_samples() draws exactly n_grains per row", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  res <-
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 10
    )

  mat_res <-
    res |>
    dplyr::select(-dataset_id_age) |>
    base::as.matrix()

  mat_orig <-
    data_wide |>
    dplyr::select(-dataset_id, -age) |>
    base::as.matrix()

  testthat::expect_equal(
    base::unname(base::rowSums(mat_res)),
    c(10, 10, 10)
  )
  testthat::expect_true(base::all(mat_res <= mat_orig))
  testthat::expect_true(base::all(mat_res >= 0))
})

testthat::test_that("rarefy_all_samples() keeps a row with exactly n_grains", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 100),
      taxon_a = c(50L, 6L),
      taxon_b = c(30L, 3L),
      taxon_c = c(20L, 1L)
    )

  res <-
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 10
    )

  vec_row_2 <-
    res |>
    dplyr::filter(dataset_id_age == "1002_100") |>
    dplyr::select(-dataset_id_age) |>
    base::unlist() |>
    base::unname()

  testthat::expect_equal(vec_row_2, c(6, 3, 1))
})

testthat::test_that("rarefy_all_samples() replaces NA counts with zero", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 100),
      taxon_a = c(NA, 3L),
      taxon_b = c(9L, NA),
      taxon_c = c(1L, 7L)
    )

  res <-
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 10
    )

  testthat::expect_equal(dplyr::pull(res, taxon_a), c(0, 3))
  testthat::expect_equal(dplyr::pull(res, taxon_b), c(9, 0))
  testthat::expect_equal(dplyr::pull(res, taxon_c), c(1, 7))
  testthat::expect_false(base::anyNA(res))
})

testthat::test_that("rarefy_all_samples() warns for rows below n_grains", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  testthat::expect_warning(
    res <-
      rarefy_all_samples(
        data_source = data_wide,
        n_grains = 50
      ),
    "row sums"
  )

  mat_res <-
    res |>
    dplyr::select(-dataset_id_age) |>
    base::as.matrix()

  testthat::expect_equal(
    base::unname(base::rowSums(mat_res)),
    c(50, 11, 10)
  )
  testthat::expect_equal(dplyr::pull(res, taxon_a)[2:3], c(10, 7))
  testthat::expect_equal(dplyr::pull(res, taxon_b)[2:3], c(0, 3))
})

testthat::test_that("rarefy_all_samples() is reproducible with a seed", {
  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  base::set.seed(900723)

  res_first <-
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 10
    )

  base::set.seed(900723)

  res_second <-
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 10
    )

  testthat::expect_equal(res_first, res_second)
  testthat::expect_equal(base::dim(res_first), c(3L, 4L))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("rarefy_all_samples() handles a single row", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = "1001",
      age = 100,
      taxon_a = 8L,
      taxon_b = 1L,
      taxon_c = 1L
    )

  res <-
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 5
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(
    base::sum(dplyr::select(res, -dataset_id_age)),
    5
  )
  testthat::expect_equal(dplyr::pull(res, dataset_id_age), "1001_100")
})

testthat::test_that("rarefy_all_samples() handles a zero-grain row", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 100),
      taxon_a = c(8L, 0L),
      taxon_b = c(2L, 0L),
      taxon_c = c(1L, 0L)
    )

  testthat::expect_warning(
    res <-
      rarefy_all_samples(
        data_source = data_wide,
        n_grains = 5
      ),
    "row sums"
  )

  testthat::expect_equal(
    base::sum(dplyr::select(dplyr::slice(res, 2), -dataset_id_age)),
    0
  )
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("rarefy_all_samples() handles 1,000 samples", {
  base::set.seed(900723)

  n_rows <- 1000L

  mat_counts <-
    base::matrix(
      stats::rpois(n_rows * 20L, lambda = 8),
      nrow = n_rows,
      ncol = 20L,
      dimnames = base::list(NULL, base::paste0("taxon_", 1:20))
    )

  data_wide <-
    tibble::tibble(
      dataset_id = base::paste0("d", base::rep(1:50, each = 20)),
      age = base::seq_len(n_rows)
    ) |>
    dplyr::bind_cols(
      tibble::as_tibble(mat_counts)
    )

  res <-
    rarefy_all_samples(
      data_source = data_wide,
      n_grains = 100
    )

  mat_res <-
    res |>
    dplyr::select(-dataset_id_age) |>
    base::as.matrix()

  testthat::expect_equal(base::dim(res), c(1000L, 21L))
  testthat::expect_equal(
    base::unname(base::rowSums(mat_res)),
    base::rep(100, n_rows)
  )
  testthat::expect_true(base::all(mat_res <= mat_counts))
  testthat::expect_equal(
    base::anyDuplicated(dplyr::pull(res, dataset_id_age)),
    0L
  )
})
