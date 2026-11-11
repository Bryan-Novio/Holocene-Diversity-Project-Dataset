#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_counts_with_ages() accepts valid inputs", {
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
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          age = c(100, 200)
        ),
        tibble::tibble(
          sample_id = "s3",
          age = 300
        )
      )
    )

  testthat::expect_no_error(
    res <-
      prepare_pollen_counts_with_ages(data_compilation = data_compilation)
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 6L)
})

testthat::test_that("prepare_pollen_counts_with_ages() needs a data frame", {
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
      prepare_pollen_counts_with_ages(data_compilation = sel_input),
      "data_compilation has to be data.frame"
    )
  }
})

testthat::test_that("prepare_pollen_counts_with_ages() validates columns", {
  data_full <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = "s1",
          abies = 1
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = "s1",
          age = 100
        )
      )
    )

  vec_required <-
    c("dataset_id", "raw_counts", "levels")

  for (
    sel_col in vec_required
  ) {
    data_missing <-
      dplyr::select(
        data_full,
        -dplyr::all_of(sel_col)
      )

    testthat::expect_error(
      prepare_pollen_counts_with_ages(data_compilation = data_missing),
      "`data_compilation` must contain"
    )
  }
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_counts_with_ages() returns a long tibble", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          abies = c(10, 20),
          alnus = c(5, 0)
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          age = c(100, 200)
        )
      )
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("dataset_id", "sample_id", "taxa", "pollen_counts", "age")
  )
  testthat::expect_equal(base::ncol(res), 5L)
  testthat::expect_equal(base::nrow(res), 4L)
  testthat::expect_type(dplyr::pull(res, pollen_counts), "double")
  testthat::expect_type(dplyr::pull(res, age), "double")
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_counts_with_ages() attaches the ages", {
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
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          age = c(100, 200)
        ),
        tibble::tibble(
          sample_id = "s3",
          age = 300
        )
      )
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  data_expected <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d1", "d2", "d2"),
      sample_id = c("s1", "s1", "s2", "s2", "s3", "s3"),
      taxa = c("abies", "alnus", "abies", "alnus", "abies", "alnus"),
      pollen_counts = c(10, 5, 20, 0, 3, 7),
      age = c(100, 100, 200, 200, 300, 300)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("prepare_pollen_counts_with_ages() drops unmatched rows", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          abies = c(1, 2)
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s2", "s9"),
          age = c(200, 900)
        )
      )
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, sample_id), "s2")
  testthat::expect_equal(dplyr::pull(res, pollen_counts), 2)
  testthat::expect_equal(dplyr::pull(res, age), 200)
})

testthat::test_that("prepare_pollen_counts_with_ages() joins within datasets", {
  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      raw_counts = base::list(
        tibble::tibble(
          sample_id = "s1",
          abies = 1
        ),
        tibble::tibble(
          sample_id = "s1",
          abies = 2
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = "s1",
          age = 100
        ),
        tibble::tibble(
          sample_id = "s1",
          age = 999
        )
      )
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_equal(dplyr::pull(res, age), c(100, 999))
  testthat::expect_equal(dplyr::pull(res, pollen_counts), c(1, 2))
})

testthat::test_that("prepare_pollen_counts_with_ages() drops NA counts", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          abies = c(NA, 2),
          alnus = c(3, NA)
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          age = c(100, 200)
        )
      )
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_false(base::anyNA(dplyr::pull(res, pollen_counts)))
  testthat::expect_equal(dplyr::pull(res, age), c(100, 200))
})

testthat::test_that("prepare_pollen_counts_with_ages() preserves totals", {
  data_compilation <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          abies = c(1, 2),
          alnus = c(3, 4)
        ),
        tibble::tibble(
          sample_id = "s3",
          abies = 5,
          alnus = 6
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          age = c(100, 200)
        ),
        tibble::tibble(
          sample_id = "s3",
          age = 300
        )
      )
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  res_counts <-
    prepare_pollen_counts(data_compilation = data_compilation)

  testthat::expect_equal(
    base::sum(dplyr::pull(res, pollen_counts)),
    base::sum(dplyr::pull(res_counts, pollen_counts))
  )
  testthat::expect_equal(base::nrow(res), base::nrow(res_counts))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_counts_with_ages() handles a single row", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = "s1",
          abies = 7
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = "s1",
          age = 50
        )
      )
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, pollen_counts), 7)
  testthat::expect_equal(dplyr::pull(res, age), 50)
})

testthat::test_that("prepare_pollen_counts_with_ages() 0 rows without match", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = "s1",
          abies = 7
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = "s2",
          age = 50
        )
      )
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c("dataset_id", "sample_id", "taxa", "pollen_counts", "age")
  )
})

testthat::test_that("prepare_pollen_counts_with_ages() keeps duplicate ages", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = "s1",
          abies = 7
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s1"),
          age = c(50, 60)
        )
      )
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_equal(dplyr::pull(res, age), c(50, 60))
})

testthat::test_that("prepare_pollen_counts_with_ages() warns on duplicate id", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = "s1",
          abies = 7,
          betula = 3
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s1"),
          age = c(50, 60)
        )
      )
    )

  testthat::expect_warning(
    res <-
      prepare_pollen_counts_with_ages(data_compilation = data_compilation),
    "many-to-many"
  )

  testthat::expect_equal(base::nrow(res), 4L)
})

testthat::test_that("prepare_pollen_counts_with_ages() keeps NA ages", {
  data_compilation <-
    tibble::tibble(
      dataset_id = "d1",
      raw_counts = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          abies = c(7, 4)
        )
      ),
      levels = base::list(
        tibble::tibble(
          sample_id = c("s1", "s2"),
          age = c(50, NA)
        )
      )
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  testthat::expect_equal(dplyr::pull(res, sample_id), c("s1", "s2"))
  testthat::expect_equal(dplyr::pull(res, age), c(50, NA))
})

testthat::test_that("prepare_pollen_counts_with_ages() handles zero rows", {
  data_compilation <-
    tibble::tibble(
      dataset_id = character(),
      raw_counts = base::list(),
      levels = base::list()
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c("dataset_id", "sample_id", "taxa", "pollen_counts", "age")
  )
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("prepare_pollen_counts_with_ages() scales to 5,000 rows", {
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

  list_levels <-
    purrr::map(
      base::seq_len(50),
      ~ tibble::tibble(
        sample_id = base::paste0("s", base::seq_len(10)),
        age = base::seq_len(10) * 100
      )
    )

  data_compilation <-
    tibble::tibble(
      dataset_id = base::paste0("d", base::seq_len(50)),
      raw_counts = list_raw,
      levels = list_levels
    )

  res <-
    prepare_pollen_counts_with_ages(data_compilation = data_compilation)

  testthat::expect_equal(base::nrow(res), 5000L)
  testthat::expect_named(
    res,
    c("dataset_id", "sample_id", "taxa", "pollen_counts", "age")
  )
  testthat::expect_false(base::anyNA(dplyr::pull(res, age)))
})
