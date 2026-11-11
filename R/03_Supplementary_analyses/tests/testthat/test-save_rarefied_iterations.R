#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_rarefied_iterations() accepts valid inputs", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  path_out <-
    withr::local_tempdir()

  testthat::expect_no_error(
    res <-
      save_rarefied_iterations(
        data_to_rarefy = data_wide,
        n_iter = 2,
        n_grains = 10,
        path = path_out
      )
  )

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 2L)
})

testthat::test_that("save_rarefied_iterations() has documented defaults", {
  list_formals <-
    base::formals(save_rarefied_iterations)

  testthat::expect_equal(
    base::names(list_formals),
    c("data_to_rarefy", "n_iter", "n_grains", "path")
  )
  testthat::expect_equal(list_formals[["n_iter"]], 1000)
  testthat::expect_equal(list_formals[["n_grains"]], 300)
})

testthat::test_that("save_rarefied_iterations() validates data_to_rarefy", {
  path_out <-
    withr::local_tempdir()

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
      save_rarefied_iterations(
        data_to_rarefy = sel_data,
        n_iter = 1,
        n_grains = 10,
        path = path_out
      ),
      "data_to_rarefy has to be a data.frame"
    )
  }

  testthat::expect_equal(base::length(base::list.files(path_out)), 0L)
})

testthat::test_that("save_rarefied_iterations() validates n_iter", {
  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 200),
      taxon_a = c(5L, 3L),
      taxon_b = c(1L, 2L)
    )

  path_out <-
    withr::local_tempdir()

  list_bad_n <-
    base::list(
      "a",
      c(1, 2),
      0,
      -1,
      1.5,
      NA_real_,
      NULL,
      numeric(0)
    )

  for (
    sel_n in list_bad_n
  ) {
    testthat::expect_error(
      save_rarefied_iterations(
        data_to_rarefy = data_wide,
        n_iter = sel_n,
        n_grains = 3,
        path = path_out
      ),
      "n_iter must be a single positive whole number"
    )
  }

  testthat::expect_equal(base::length(base::list.files(path_out)), 0L)
})

testthat::test_that("save_rarefied_iterations() validates n_grains", {
  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 200),
      taxon_a = c(5L, 3L),
      taxon_b = c(1L, 2L)
    )

  path_out <-
    withr::local_tempdir()

  list_bad_n <-
    base::list(
      "a",
      c(3, 4),
      0,
      -1,
      2.5,
      NA_real_,
      Inf,
      NULL,
      numeric(0)
    )

  for (
    sel_n in list_bad_n
  ) {
    testthat::expect_error(
      save_rarefied_iterations(
        data_to_rarefy = data_wide,
        n_iter = 1,
        n_grains = sel_n,
        path = path_out
      ),
      "n_grains must be a single positive whole number"
    )
  }

  testthat::expect_equal(base::length(base::list.files(path_out)), 0L)
})

testthat::test_that("save_rarefied_iterations() validates path", {
  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1002"),
      age = c(100, 200),
      taxon_a = c(5L, 3L),
      taxon_b = c(1L, 2L)
    )

  path_out <-
    withr::local_tempdir()

  path_file <-
    base::file.path(path_out, "not_a_dir.txt")

  base::file.create(path_file)

  path_missing <-
    base::file.path(path_out, "missing_dir")

  list_bad_path <-
    base::list(
      1,
      c(path_out, path_out),
      NA_character_,
      NULL,
      path_missing,
      path_file
    )

  for (
    sel_path in list_bad_path
  ) {
    testthat::expect_error(
      save_rarefied_iterations(
        data_to_rarefy = data_wide,
        n_iter = 1,
        n_grains = 3,
        path = sel_path
      ),
      "path must be a single existing directory"
    )
  }

  testthat::expect_false(base::dir.exists(path_missing))
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("save_rarefied_iterations() returns paths invisibly", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  path_out <-
    withr::local_tempdir()

  list_visible <-
    base::withVisible(
      save_rarefied_iterations(
        data_to_rarefy = data_wide,
        n_iter = 3,
        n_grains = 10,
        path = path_out
      )
    )

  testthat::expect_false(list_visible[["visible"]])
  testthat::expect_equal(
    list_visible[["value"]],
    base::file.path(path_out, c("1.rds", "2.rds", "3.rds"))
  )
})


#----------------------------------------------------------#
# Side effects -----
#----------------------------------------------------------#

testthat::test_that("save_rarefied_iterations() writes a file per iteration", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  path_out <-
    withr::local_tempdir()

  res <-
    save_rarefied_iterations(
      data_to_rarefy = data_wide,
      n_iter = 4,
      n_grains = 10,
      path = path_out
    )

  testthat::expect_equal(
    base::sort(base::list.files(path_out)),
    base::sort(c("1.rds", "2.rds", "3.rds", "4.rds"))
  )
  testthat::expect_true(base::all(base::file.exists(res)))

  list_files <-
    base::lapply(res, readr::read_rds)

  for (
    sel_data in list_files
  ) {
    testthat::expect_s3_class(sel_data, "tbl_df")
    testthat::expect_named(
      sel_data,
      c("dataset_id_age", "taxon_a", "taxon_b", "taxon_c")
    )
    testthat::expect_equal(base::nrow(sel_data), 3L)
  }
})

testthat::test_that("save_rarefied_iterations() honours n_grains", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  path_out <-
    withr::local_tempdir()

  res <-
    save_rarefied_iterations(
      data_to_rarefy = data_wide,
      n_iter = 2,
      n_grains = 5,
      path = path_out
    )

  for (
    sel_file in res
  ) {
    mat_counts <-
      sel_file |>
      readr::read_rds() |>
      dplyr::select(-dataset_id_age) |>
      base::as.matrix()

    testthat::expect_equal(
      base::unname(base::rowSums(mat_counts)),
      c(5, 5, 5)
    )
  }
})

testthat::test_that("save_rarefied_iterations() skips non-empty files", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  path_out <-
    withr::local_tempdir()

  path_existing <-
    base::file.path(path_out, "1.rds")

  readr::write_rds("sentinel", file = path_existing)

  num_size_before <-
    base::file.info(path_existing)[["size"]]

  res <-
    save_rarefied_iterations(
      data_to_rarefy = data_wide,
      n_iter = 2,
      n_grains = 10,
      path = path_out
    )

  testthat::expect_equal(readr::read_rds(path_existing), "sentinel")
  testthat::expect_equal(
    base::file.info(path_existing)[["size"]],
    num_size_before
  )
  testthat::expect_s3_class(readr::read_rds(res[2]), "tbl_df")
  testthat::expect_equal(base::length(base::list.files(path_out)), 2L)
})

testthat::test_that("save_rarefied_iterations() overwrites empty files", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  path_out <-
    withr::local_tempdir()

  path_empty <-
    base::file.path(path_out, "1.rds")

  base::file.create(path_empty)

  testthat::expect_equal(base::file.info(path_empty)[["size"]], 0)

  save_rarefied_iterations(
    data_to_rarefy = data_wide,
    n_iter = 1,
    n_grains = 10,
    path = path_out
  )

  testthat::expect_true(base::file.info(path_empty)[["size"]] > 0)
  testthat::expect_s3_class(readr::read_rds(path_empty), "tbl_df")
})

testthat::test_that("save_rarefied_iterations() is reproducible with a seed", {
  data_wide <-
    tibble::tibble(
      dataset_id = c("1001", "1001", "1002"),
      age = c(100, 200, 100),
      taxon_a = c(50L, 10L, 7L),
      taxon_b = c(30L, 0L, 3L),
      taxon_c = c(20L, 1L, 0L)
    )

  path_first <-
    withr::local_tempdir()

  path_second <-
    withr::local_tempdir()

  base::set.seed(900723)

  res_first <-
    save_rarefied_iterations(
      data_to_rarefy = data_wide,
      n_iter = 3,
      n_grains = 10,
      path = path_first
    )

  base::set.seed(900723)

  res_second <-
    save_rarefied_iterations(
      data_to_rarefy = data_wide,
      n_iter = 3,
      n_grains = 10,
      path = path_second
    )

  list_first <-
    base::lapply(res_first, readr::read_rds)

  testthat::expect_equal(
    list_first,
    base::lapply(res_second, readr::read_rds)
  )
  testthat::expect_length(list_first, 3L)
  testthat::expect_equal(base::dim(list_first[[1]]), c(3L, 4L))
})

testthat::test_that("save_rarefied_iterations() iterations differ", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = "1001",
      age = 100,
      taxon_a = 100L,
      taxon_b = 100L,
      taxon_c = 100L,
      taxon_d = 1L
    )

  path_out <-
    withr::local_tempdir()

  res <-
    save_rarefied_iterations(
      data_to_rarefy = data_wide,
      n_iter = 5,
      n_grains = 30,
      path = path_out
    )

  list_files <-
    base::lapply(res, readr::read_rds)

  testthat::expect_true(base::length(base::unique(list_files)) > 1L)
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("save_rarefied_iterations() handles a single iteration", {
  base::set.seed(900723)

  data_wide <-
    tibble::tibble(
      dataset_id = "1001",
      age = 100,
      taxon_a = 8L,
      taxon_b = 1L,
      taxon_c = 1L
    )

  path_out <-
    withr::local_tempdir()

  res <-
    save_rarefied_iterations(
      data_to_rarefy = data_wide,
      n_iter = 1,
      n_grains = 5,
      path = path_out
    )

  testthat::expect_equal(res, base::file.path(path_out, "1.rds"))
  testthat::expect_equal(base::list.files(path_out), "1.rds")
})

testthat::test_that("save_rarefied_iterations() returns skipped paths too", {
  data_wide <-
    tibble::tibble(
      dataset_id = "1001",
      age = 100,
      taxon_a = 8L,
      taxon_b = 1L,
      taxon_c = 1L
    )

  path_out <-
    withr::local_tempdir()

  readr::write_rds("a", file = base::file.path(path_out, "1.rds"))
  readr::write_rds("b", file = base::file.path(path_out, "2.rds"))

  res <-
    save_rarefied_iterations(
      data_to_rarefy = data_wide,
      n_iter = 2,
      n_grains = 5,
      path = path_out
    )

  testthat::expect_equal(
    res,
    base::file.path(path_out, c("1.rds", "2.rds"))
  )
  testthat::expect_equal(
    readr::read_rds(base::file.path(path_out, "2.rds")),
    "b"
  )
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("save_rarefied_iterations() handles 1,000 samples", {
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

  path_out <-
    withr::local_tempdir()

  res <-
    save_rarefied_iterations(
      data_to_rarefy = data_wide,
      n_iter = 5,
      n_grains = 100,
      path = path_out
    )

  data_first <-
    readr::read_rds(res[1])

  testthat::expect_length(res, 5L)
  testthat::expect_equal(base::dim(data_first), c(1000L, 21L))
  testthat::expect_equal(
    base::unname(base::rowSums(dplyr::select(data_first, -dataset_id_age))),
    base::rep(100, n_rows)
  )
})
