#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("summarise_n_distinct() accepts valid inputs", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2"),
      sample_id = c("s1", "s2", "s3"),
      taxa = c("a", "b", "a"),
      region = c("r1", "r1", "r2")
    )

  testthat::expect_no_error(
    res <-
      summarise_n_distinct(data_source = data_source)
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(dplyr::pull(res, n), 2L)
})

testthat::test_that("summarise_n_distinct() rejects non-data-frames", {
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
      summarise_n_distinct(data_source = sel_input),
      "`data_source` must be a data frame\\."
    )
  }
})

testthat::test_that("summarise_n_distinct() validates metric", {
  data_source <-
    tibble::tibble(
      dataset_id = "d1",
      sample_id = "s1",
      taxa = "a"
    )

  list_bad_metric <-
    base::list(
      "not_a_metric",
      "region",
      1,
      c("sample_id", "not_a_metric")
    )

  for (
    sel_metric in list_bad_metric
  ) {
    testthat::expect_error(
      summarise_n_distinct(
        data_source = data_source,
        metric = sel_metric
      ),
      "'arg'"
    )
  }
})

testthat::test_that("summarise_n_distinct() requires the metric column", {
  data_source <-
    tibble::tibble(
      dataset_id = "d1",
      sample_id = "s1"
    )

  testthat::expect_error(
    summarise_n_distinct(
      data_source = data_source,
      metric = "taxa"
    ),
    "The `metric` column must exist in `data_source`\\."
  )
})

testthat::test_that("summarise_n_distinct() validates group_var", {
  data_source <-
    tibble::tibble(
      dataset_id = "d1",
      sample_id = "s1",
      taxa = "a",
      region = "r1"
    )

  testthat::expect_error(
    summarise_n_distinct(
      data_source = data_source,
      group_var = 1
    ),
    "`group_var` must be NULL or a character vector\\."
  )

  testthat::expect_error(
    summarise_n_distinct(
      data_source = data_source,
      group_var = "not_a_column"
    ),
    "All specified group_var columns must exist"
  )

  testthat::expect_error(
    summarise_n_distinct(
      data_source = data_source,
      group_var = c("region", "not_a_column")
    ),
    "All specified group_var columns must exist"
  )
})

testthat::test_that("summarise_n_distinct() validates name", {
  data_source <-
    tibble::tibble(
      dataset_id = "d1",
      sample_id = "s1",
      taxa = "a"
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
      summarise_n_distinct(
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

testthat::test_that("summarise_n_distinct() returns one row by default", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2"),
      sample_id = c("s1", "s2", "s3"),
      taxa = c("a", "b", "a")
    )

  res <-
    summarise_n_distinct(data_source = data_source)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, "n")
  testthat::expect_equal(base::dim(res), c(1L, 1L))
  testthat::expect_type(dplyr::pull(res, n), "integer")
})

testthat::test_that("summarise_n_distinct() returns group columns", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2", "d3"),
      sample_id = c("s1", "s2", "s3", "s4"),
      taxa = c("a", "b", "a", "a"),
      region = c("r1", "r1", "r2", "r2"),
      climate = c("c1", "c2", "c1", "c1")
    )

  res_one <-
    summarise_n_distinct(
      data_source = data_source,
      group_var = "region"
    )

  res_two <-
    summarise_n_distinct(
      data_source = data_source,
      group_var = c("region", "climate"),
      name = "all"
    )

  testthat::expect_named(res_one, c("region", "n"))
  testthat::expect_named(res_two, c("region", "climate", "n", "data"))
  testthat::expect_equal(base::dim(res_one), c(2L, 2L))
  testthat::expect_equal(base::dim(res_two), c(3L, 4L))
})

testthat::test_that("summarise_n_distinct() returns an ungrouped tibble", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      region = c("r1", "r2"),
      climate = c("c1", "c1")
    )

  res <-
    summarise_n_distinct(
      data_source = data_source,
      group_var = c("region", "climate")
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, c("region", "climate", "n"))
  testthat::expect_false(dplyr::is_grouped_df(res))
})

testthat::test_that("summarise_n_distinct() adds the data column", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d2")
    )

  res <-
    summarise_n_distinct(
      data_source = data_source,
      name = "my_data"
    )

  testthat::expect_named(res, c("n", "data"))
  testthat::expect_equal(dplyr::pull(res, data), "my_data")
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("summarise_n_distinct() counts each metric", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a")
    )

  res_default <-
    summarise_n_distinct(data_source = data_source)

  res_dataset <-
    summarise_n_distinct(
      data_source = data_source,
      metric = "dataset_id"
    )

  res_sample <-
    summarise_n_distinct(
      data_source = data_source,
      metric = "sample_id"
    )

  res_taxa <-
    summarise_n_distinct(
      data_source = data_source,
      metric = "taxa"
    )

  testthat::expect_equal(dplyr::pull(res_default, n), 3L)
  testthat::expect_equal(dplyr::pull(res_dataset, n), 3L)
  testthat::expect_equal(dplyr::pull(res_sample, n), 4L)
  testthat::expect_equal(dplyr::pull(res_taxa, n), 3L)
})

testthat::test_that("summarise_n_distinct() accepts partial metric names", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      sample_id = c("s1", "s1"),
      taxa = c("a", "b")
    )

  res <-
    summarise_n_distinct(
      data_source = data_source,
      metric = "samp"
    )

  testthat::expect_equal(dplyr::pull(res, n), 1L)
})

testthat::test_that("summarise_n_distinct() counts per single group", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2")
    )

  res_dataset <-
    summarise_n_distinct(
      data_source = data_source,
      metric = "dataset_id",
      group_var = "region"
    )

  res_sample <-
    summarise_n_distinct(
      data_source = data_source,
      metric = "sample_id",
      group_var = "region",
      name = "all"
    )

  testthat::expect_equal(dplyr::pull(res_dataset, region), c("r1", "r2"))
  testthat::expect_equal(dplyr::pull(res_dataset, n), c(1L, 2L))
  testthat::expect_equal(dplyr::pull(res_sample, n), c(2L, 2L))
  testthat::expect_equal(dplyr::pull(res_sample, data), c("all", "all"))
})

testthat::test_that("summarise_n_distinct() counts per multiple groups", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d1", "d2", "d2", "d3"),
      sample_id = c("s1", "s1", "s2", "s3", "s3", "s4"),
      taxa = c("a", "b", "a", "a", "c", "a"),
      region = c("r1", "r1", "r1", "r2", "r2", "r2"),
      climate = c("c1", "c1", "c2", "c1", "c1", "c1")
    )

  res <-
    summarise_n_distinct(
      data_source = data_source,
      metric = "taxa",
      group_var = c("region", "climate")
    )

  testthat::expect_equal(dplyr::pull(res, region), c("r1", "r1", "r2"))
  testthat::expect_equal(dplyr::pull(res, climate), c("c1", "c2", "c1"))
  testthat::expect_equal(dplyr::pull(res, n), c(2L, 1L, 2L))
})

testthat::test_that("summarise_n_distinct() drops NA values (na.rm)", {
  data_source <-
    tibble::tibble(
      dataset_id = c("d1", NA, NA, "d2"),
      sample_id = c("s1", "s2", "s3", NA)
    )

  res_dataset <-
    summarise_n_distinct(
      data_source = data_source,
      metric = "dataset_id"
    )

  res_sample <-
    summarise_n_distinct(
      data_source = data_source,
      metric = "sample_id"
    )

  testthat::expect_equal(dplyr::pull(res_dataset, n), 2L)
  testthat::expect_equal(dplyr::pull(res_sample, n), 3L)
})

testthat::test_that("summarise_n_distinct() counts distinct, not rows", {
  data_source <-
    tibble::tibble(
      dataset_id = base::rep("d1", times = 5)
    )

  res <-
    summarise_n_distinct(data_source = data_source)

  testthat::expect_equal(dplyr::pull(res, n), 1L)
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("summarise_n_distinct() handles a single row", {
  data_source <-
    tibble::tibble(
      dataset_id = "d1",
      region = "r1"
    )

  res <-
    summarise_n_distinct(
      data_source = data_source,
      group_var = "region"
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, n), 1L)
})

testthat::test_that("summarise_n_distinct() handles zero rows", {
  data_source <-
    tibble::tibble(
      dataset_id = character(),
      region = character()
    )

  res_overall <-
    summarise_n_distinct(data_source = data_source)

  res_group <-
    summarise_n_distinct(
      data_source = data_source,
      group_var = "region"
    )

  testthat::expect_equal(dplyr::pull(res_overall, n), 0L)
  testthat::expect_equal(base::nrow(res_group), 0L)
  testthat::expect_named(res_group, c("region", "n"))
})

testthat::test_that("summarise_n_distinct() handles an all-NA metric", {
  data_source <-
    tibble::tibble(
      dataset_id = c(NA_character_, NA_character_)
    )

  res <-
    summarise_n_distinct(data_source = data_source)

  testthat::expect_equal(dplyr::pull(res, n), 0L)
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("summarise_n_distinct() scales to 5,000 rows", {
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
    summarise_n_distinct(
      data_source = data_source,
      metric = "sample_id"
    )

  res_group <-
    summarise_n_distinct(
      data_source = data_source,
      metric = "dataset_id",
      group_var = "region"
    )

  testthat::expect_equal(base::nrow(data_source), 5000L)
  testthat::expect_equal(dplyr::pull(res_overall, n), 500L)
  testthat::expect_equal(dplyr::pull(res_group, n), c(25L, 25L))
})
