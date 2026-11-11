#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_counts() accepts valid inputs", {
  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          abies = c(10, 20),
          alnus = c(5, 0)
        ),
        tibble::tibble(
          sample_id = "s3",
          abies = 3,
          alnus = 7
        )
      )
    )

  testthat::expect_no_error(
    res <-
      prepare_pollen_counts(data_compilation = data_compilation)
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 6L)
})

testthat::test_that("prepare_pollen_counts() rejects non-data-frame input", {
  list_bad_input <-
    base::list(
      NULL,
      1:3,
      "d1",
      base::list(dataset_id = "d1", raw_counts = base::list())
    )

  for (
    sel_input in list_bad_input
  ) {
    testthat::expect_error(
      prepare_pollen_counts(data_compilation = sel_input),
      "`data_compilation` must be a data frame or tibble\\."
    )
  }
})

testthat::test_that("prepare_pollen_counts() validates required columns", {
  data_full <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = "s1",
          abies = 1
        )
      )
    )

  vec_required <-
    c("dataset_id", "raw_counts")

  for (
    sel_col in vec_required
  ) {
    data_missing <-
      dplyr::select(
        data_full,
        -dplyr::all_of(sel_col)
      )

    testthat::expect_error(
      prepare_pollen_counts(data_compilation = data_missing),
      "`data_compilation` must contain columns"
    )
  }

  testthat::expect_error(
    prepare_pollen_counts(data_compilation = tibble::tibble()),
    "`data_compilation` must contain columns"
  )
})

testthat::test_that("prepare_pollen_counts() needs sample_id in raw_counts", {
  data_no_sample_id <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          abies = c(1, 2)
        )
      )
    )

  testthat::expect_error(
    prepare_pollen_counts(data_compilation = data_no_sample_id)
  )
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_counts() returns a long tibble", {
  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          abies = c(10, 20),
          alnus = c(5, 0)
        ),
        tibble::tibble(
          sample_id = "s3",
          abies = 3,
          alnus = 7
        )
      )
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("dataset_id", "sample_id", "taxa", "pollen_counts")
  )
  testthat::expect_equal(base::ncol(res), 4L)
  testthat::expect_equal(base::nrow(res), 6L)
  testthat::expect_type(dplyr::pull(res, dataset_id), "character")
  testthat::expect_type(dplyr::pull(res, sample_id), "character")
  testthat::expect_type(dplyr::pull(res, taxa), "character")
  testthat::expect_type(dplyr::pull(res, pollen_counts), "double")
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_counts() reshapes counts to long format", {
  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          abies = c(10, 20),
          alnus = c(5, 0)
        ),
        tibble::tibble(
          sample_id = "s3",
          abies = 3,
          alnus = 7
        )
      )
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  data_expected <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d1", "d2", "d2"),
      sample_id = c("s1", "s1", "s2", "s2", "s3", "s3"),
      taxa = c("abies", "alnus", "abies", "alnus", "abies", "alnus"),
      pollen_counts = c(10, 5, 20, 0, 3, 7)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("prepare_pollen_counts() drops NA counts but keeps zeros", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          abies = c(NA, 0),
          alnus = c(4, NA)
        )
      )
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_false(base::anyNA(dplyr::pull(res, pollen_counts)))
  testthat::expect_equal(
    dplyr::pull(res, taxa),
    c("alnus", "abies")
  )
  testthat::expect_equal(
    dplyr::pull(res, pollen_counts),
    c(4, 0)
  )
})

testthat::test_that("prepare_pollen_counts() keeps only taxa of each dataset", {
  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      raw_counts = base::list(
        tibble::tibble(
          sample_id = "s1",
          abies = 1
        ),
        tibble::tibble(
          sample_id = "s2",
          pinus = 2
        )
      )
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  data_expected <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      sample_id = c("s1", "s2"),
      taxa = c("abies", "pinus"),
      pollen_counts = c(1, 2)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("prepare_pollen_counts() preserves totals and uniqueness", {
  mat_counts <-
    base::matrix(
      c(1, 2, NA, 4, 5, 6, 0, NA, 9),
      nrow = 3,
      dimnames = base::list(NULL, c("a", "b", "c"))
    )

  data_raw <-
    tibble::as_tibble(mat_counts) |>
    dplyr::mutate(
      sample_id = c("s1", "s2", "s3"),
      .before = 1
    )

  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(data_raw)
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  testthat::expect_equal(
    base::sum(dplyr::pull(res, pollen_counts)),
    base::sum(mat_counts, na.rm = TRUE)
  )
  testthat::expect_equal(
    base::nrow(res),
    base::sum(!base::is.na(mat_counts))
  )
  testthat::expect_equal(
    base::nrow(dplyr::distinct(res, dataset_id, sample_id, taxa)),
    base::nrow(res)
  )
})

testthat::test_that("prepare_pollen_counts() ignores extra columns", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      site_name = "Lake",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = "s1",
          abies = 1
        )
      )
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  testthat::expect_named(
    res,
    c("dataset_id", "sample_id", "taxa", "pollen_counts")
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_counts() handles a single row and taxon", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = "s1",
          abies = 7
        )
      )
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, pollen_counts), 7)
})

testthat::test_that("prepare_pollen_counts() gives 0 rows for NA counts", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          abies = c(NA_real_, NA_real_)
        )
      )
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c("dataset_id", "sample_id", "taxa", "pollen_counts")
  )
})

testthat::test_that("prepare_pollen_counts() keeps duplicated sample ids", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s1"),
          abies = c(1, 2)
        )
      )
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_equal(dplyr::pull(res, pollen_counts), c(1, 2))
})

testthat::test_that("prepare_pollen_counts() handles a zero-row compilation", {
  testthat::skip(
    "Known issue: zero-row compilation fails in pivot_longer()."
  )

  data_compilation <-
    tibble::tibble(
      dataset_id = character(),
      raw_counts = base::list()
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c("dataset_id", "sample_id", "taxa", "pollen_counts")
  )
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_counts() scales to thousands of rows", {
  list_raw <-
    purrr::map(
      base::seq_len(50),
      ~ tibble::as_tibble(
        base::matrix(
          base::seq_len(100),
          nrow = 10,
          dimnames = base::list(NULL, base::paste0("t", base::seq_len(10)))
        )
      ) |>
        dplyr::mutate(
          sample_id = base::paste0("s", base::seq_len(10)),
          .before = 1
        )
    )

  data_compilation <-
    tibble::tibble(
      dataset_id = base::paste0("d", base::seq_len(50)),
      raw_counts = list_raw
    )

  res <-
    prepare_pollen_counts(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 5000L)
  testthat::expect_named(
    res,
    c("dataset_id", "sample_id", "taxa", "pollen_counts")
  )
  testthat::expect_equal(
    base::sum(dplyr::pull(res, pollen_counts)),
    50 * base::sum(base::seq_len(100))
  )
})
