#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("summarise_n_samples() accepts valid inputs", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  testthat::expect_no_error(
    res <-
      summarise_n_samples(data_source = data_source)
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(dplyr::pull(res, n), 4L)
})

testthat::test_that("summarise_n_samples() rejects non-data-frame input", {
  list_bad_input <-
    base::list(
      NULL,
      1:3,
      "d1",
      base::list(sample_id = "x")
    )

  for (
    sel_input in list_bad_input
  ) {
    testthat::expect_error(
      summarise_n_samples(data_source = sel_input),
      "`data_source` must be a data frame\\."
    )
  }
})

testthat::test_that("summarise_n_samples() requires the counted column", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  data_missing <-
    dplyr::select(
      data_source,
      -dplyr::all_of("sample_id")
    )

  testthat::expect_error(
    summarise_n_samples(data_source = data_missing),
    "`data_source` must contain the column 'sample_id'"
  )
})

testthat::test_that("summarise_n_samples() validates group_var", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  testthat::expect_error(
    summarise_n_samples(
      data_source = data_source,
      group_var = 1
    ),
    "`group_var` must be NULL or a character vector\\."
  )

  testthat::expect_error(
    summarise_n_samples(
      data_source = data_source,
      group_var = "not_a_column"
    ),
    "All `group_var` columns must exist in `data_source`\\."
  )

  testthat::expect_error(
    summarise_n_samples(
      data_source = data_source,
      group_var = c("region", "not_a_column")
    ),
    "All `group_var` columns must exist in `data_source`\\."
  )
})

testthat::test_that("summarise_n_samples() validates name", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  list_bad_name <-
    base::list(
      1,
      TRUE,
      c("a", "b"),
      character(0),
      NA_character_
    )

  for (
    sel_name in list_bad_name
  ) {
    testthat::expect_error(
      summarise_n_samples(
        data_source = data_source,
        name = sel_name
      ),
      "`name` must be NULL or a single character string\\."
    )
  }
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("summarise_n_samples() returns one row by default", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  res <-
    summarise_n_samples(data_source = data_source)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, "n")
  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(base::ncol(res), 1L)
  testthat::expect_type(dplyr::pull(res, n), "integer")
})

testthat::test_that("summarise_n_samples() names group columns correctly", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  res_one <-
    summarise_n_samples(
      data_source = data_source,
      group_var = "region"
    )

  res_two <-
    summarise_n_samples(
      data_source = data_source,
      group_var = c("region", "climate"),
      name = "all"
    )

  testthat::expect_named(res_one, c("region", "n"))
  testthat::expect_named(res_two, c("region", "climate", "n", "data"))
  testthat::expect_false("get(group_var)" %in% base::colnames(res_one))
  testthat::expect_equal(base::dim(res_one), c(2L, 2L))
  testthat::expect_equal(base::dim(res_two), c(3L, 4L))
})

testthat::test_that("summarise_n_samples() adds the data column", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  res <-
    summarise_n_samples(
      data_source = data_source,
      name = "my_data"
    )

  testthat::expect_named(res, c("n", "data"))
  testthat::expect_type(dplyr::pull(res, data), "character")
  testthat::expect_equal(dplyr::pull(res, data), "my_data")
})

testthat::test_that("summarise_n_samples() keeps the result grouped", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  res <-
    summarise_n_samples(
      data_source = data_source,
      group_var = c("region", "climate")
    )

  testthat::expect_equal(dplyr::group_vars(res), c("region", "climate"))
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("summarise_n_samples() counts distinct overall", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  res <-
    summarise_n_samples(data_source = data_source)

  testthat::expect_equal(dplyr::pull(res, n), 4L)
})

testthat::test_that("summarise_n_samples() counts distinct per group", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  res <-
    summarise_n_samples(
      data_source = data_source,
      group_var = "region",
      name = "all"
    )

  testthat::expect_equal(dplyr::pull(res, region), c("r1", "r2"))
  testthat::expect_equal(dplyr::pull(res, n), c(2L, 2L))
  testthat::expect_equal(dplyr::pull(res, data), c("all", "all"))
})

testthat::test_that("summarise_n_samples() counts by two group variables", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  res <-
    summarise_n_samples(
      data_source = data_source,
      group_var = c("region", "climate")
    )

  testthat::expect_equal(dplyr::pull(res, region), c("r1", "r1", "r2"))
  testthat::expect_equal(dplyr::pull(res, climate), c("c1", "c2", "c1"))
  testthat::expect_equal(dplyr::pull(res, n), c(1L, 1L, 2L))
})

testthat::test_that("summarise_n_samples() counts NA as a distinct value", {
  data_source <-
    tibble::tibble(
      sample_id = c("x1", NA, NA, "x2")
    )

  res <-
    summarise_n_samples(data_source = data_source)

  testthat::expect_equal(dplyr::pull(res, n), 3L)
})

testthat::test_that("summarise_n_samples() counts distinct, not rows", {
  data_source <-
    tibble::tibble(
      sample_id = base::rep("x", times = 5)
    )

  res <-
    summarise_n_samples(data_source = data_source)

  testthat::expect_equal(dplyr::pull(res, n), 1L)
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("summarise_n_samples() handles a single row", {
  data_source <-
    tibble::tibble(
      dataset_id = "d1",
      sample_id = "s1",
      taxa = "a",
      region = "r1"
    )

  res <-
    summarise_n_samples(
      data_source = data_source,
      group_var = "region"
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, n), 1L)
})

testthat::test_that("summarise_n_samples() handles zero rows", {
  data_source <-
    tibble::tibble(
      dataset_id = character(),
      sample_id = character(),
      taxa = character(),
      region = character()
    )

  res_overall <-
    summarise_n_samples(data_source = data_source)

  res_group <-
    summarise_n_samples(
      data_source = data_source,
      group_var = "region"
    )

  testthat::expect_equal(dplyr::pull(res_overall, n), 0L)
  testthat::expect_equal(base::nrow(res_group), 0L)
  testthat::expect_named(res_group, c("region", "n"))
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("summarise_n_samples() scales to 5,000 rows", {
  data_source <-
    tidyr::expand_grid(
      dataset_id = base::paste0("d", base::seq_len(50)),
      sample_no = base::seq_len(10),
      taxa = base::paste0("t", base::seq_len(10))
    ) |>
    dplyr::mutate(
      sample_id = base::paste0(dataset_id, "_s", sample_no),
      region = base::rep(c("r1", "r2"), each = 2500)
    )

  res_overall <-
    summarise_n_samples(data_source = data_source)

  res_group <-
    summarise_n_samples(
      data_source = data_source,
      group_var = "region"
    )

  testthat::expect_equal(base::nrow(data_source), 5000L)
  testthat::expect_equal(dplyr::pull(res_overall, n), 500L)
  testthat::expect_equal(dplyr::pull(res_group, n), c(250L, 250L))
})
