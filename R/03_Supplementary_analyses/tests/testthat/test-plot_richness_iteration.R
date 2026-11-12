#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("plot_richness_iteration() accepts valid inputs", {
  dir_data <-
    withr::local_tempdir()

  data_back <-
    tibble::tibble(
      region = c("A", "A", "B", "B"),
      age = c(0, 1000, 0, 1000),
      richness = c(1, 2, 3, 4),
      rich_low = c(0, 1, 2, 3),
      rich_high = c(2, 3, 4, 5)
    )

  data_rich <-
    tibble::tibble(
      dataset_id = c(1, 1, 2, 2),
      age = c(0, 1000, 0, 1000),
      richness = c(1, 2, 3, 4),
      region = c("A", "A", "B", "B")
    )

  base::saveRDS(data_back, file = base::file.path(dir_data, "back_1.rds"))
  base::saveRDS(data_rich, file = base::file.path(dir_data, "rich_1.rds"))

  vec_back <-
    base::file.path(dir_data, "back_1.rds")

  vec_rich <-
    base::file.path(dir_data, "rich_1.rds")

  testthat::expect_no_error(
    plot_iter <-
      plot_richness_iteration(
        data_back = vec_back,
        data_rich = vec_rich,
        iter = 1
      )
  )

  testthat::expect_true(ggplot2::is_ggplot(plot_iter))
})

testthat::test_that("plot_richness_iteration() validates data_back", {
  list_bad_back <-
    base::list(
      NULL,
      1,
      TRUE,
      base::as.environment(base::list(a = 1))
    )

  for (
    sel_bad in list_bad_back
  ) {
    testthat::expect_error(
      plot_richness_iteration(
        data_back = sel_bad,
        data_rich = "rich_1.rds",
        iter = 1
      ),
      "`data_back` must be a character vector or a list of file paths\\."
    )
  }
})

testthat::test_that("plot_richness_iteration() validates data_rich", {
  list_bad_rich <-
    base::list(
      NULL,
      1,
      TRUE,
      base::as.environment(base::list(a = 1))
    )

  for (
    sel_bad in list_bad_rich
  ) {
    testthat::expect_error(
      plot_richness_iteration(
        data_back = "back_1.rds",
        data_rich = sel_bad,
        iter = 1
      ),
      "`data_rich` must be a character vector or a list of file paths\\."
    )
  }
})

testthat::test_that("plot_richness_iteration() validates the type of iter", {
  list_bad_iter <-
    base::list(
      c(1, 2),
      NA,
      NA_real_,
      NA_character_,
      TRUE,
      base::list(1),
      NULL,
      numeric(0)
    )

  for (
    sel_bad in list_bad_iter
  ) {
    testthat::expect_error(
      plot_richness_iteration(
        data_back = "back_1.rds",
        data_rich = "rich_1.rds",
        iter = sel_bad
      ),
      "`iter` must be a single number or string\\."
    )
  }
})

testthat::test_that("plot_richness_iteration() validates the range of iter", {
  vec_back <-
    c("back_1.rds", "back_2.rds")

  vec_rich <-
    c("rich_1.rds", "rich_2.rds")

  list_bad_iter <-
    base::list(0, 3, -1, 1.5, "unknown_name")

  for (
    sel_bad in list_bad_iter
  ) {
    testthat::expect_error(
      plot_richness_iteration(
        data_back = vec_back,
        data_rich = vec_rich,
        iter = sel_bad
      ),
      "`iter` must be an index or a name present in both inputs\\."
    )
  }

  testthat::expect_error(
    plot_richness_iteration(
      data_back = vec_back,
      data_rich = "rich_1.rds",
      iter = 2
    ),
    "`iter` must be an index or a name present in both inputs\\."
  )
})

testthat::test_that("plot_richness_iteration() checks that files exist", {
  dir_data <-
    withr::local_tempdir()

  data_back <-
    tibble::tibble(
      region = "A",
      age = 0,
      richness = 1,
      rich_low = 0,
      rich_high = 2
    )

  base::saveRDS(data_back, file = base::file.path(dir_data, "back_1.rds"))

  testthat::expect_error(
    plot_richness_iteration(
      data_back = base::file.path(dir_data, "missing.rds"),
      data_rich = base::file.path(dir_data, "back_1.rds"),
      iter = 1
    ),
    "The selected file of `data_back` does not exist\\."
  )

  testthat::expect_error(
    plot_richness_iteration(
      data_back = base::file.path(dir_data, "back_1.rds"),
      data_rich = base::file.path(dir_data, "missing.rds"),
      iter = 1
    ),
    "The selected file of `data_rich` does not exist\\."
  )
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("plot_richness_iteration() returns lines and facets", {
  dir_data <-
    withr::local_tempdir()

  data_back <-
    tibble::tibble(
      region = c("A", "A", "B", "B"),
      age = c(0, 1000, 0, 1000),
      richness = c(1, 2, 3, 4),
      rich_low = c(0, 1, 2, 3),
      rich_high = c(2, 3, 4, 5)
    )

  data_rich <-
    tibble::tibble(
      dataset_id = c(1, 1, 2, 2),
      age = c(0, 1000, 0, 1000),
      richness = c(1, 2, 3, 4),
      region = c("A", "A", "B", "B")
    )

  base::saveRDS(data_back, file = base::file.path(dir_data, "back_1.rds"))
  base::saveRDS(data_rich, file = base::file.path(dir_data, "rich_1.rds"))

  plot_iter <-
    plot_richness_iteration(
      data_back = base::file.path(dir_data, "back_1.rds"),
      data_rich = base::file.path(dir_data, "rich_1.rds"),
      iter = 1
    )

  vec_geoms <-
    purrr::map_chr(
      .x = plot_iter@layers,
      .f = ~ base::class(.x[["geom"]])[1]
    )

  scale_x <-
    plot_iter@scales[["get_scales"]]("x")

  testthat::expect_true(ggplot2::is_ggplot(plot_iter))
  testthat::expect_length(plot_iter@layers, 3L)
  testthat::expect_equal(
    base::unname(vec_geoms),
    c("GeomLine", "GeomLine", "GeomRibbon")
  )
  testthat::expect_s3_class(plot_iter@facet, "FacetWrap")
  testthat::expect_named(plot_iter@facet[["params"]][["facets"]], "region")
  testthat::expect_equal(scale_x[["trans"]][["name"]], "reverse")
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("plot_richness_iteration() uses the data of both files", {
  dir_data <-
    withr::local_tempdir()

  data_back <-
    tibble::tibble(
      region = c("A", "A", "B", "B"),
      age = c(0, 1000, 0, 1000),
      richness = c(1, 2, 3, 4),
      rich_low = c(0, 1, 2, 3),
      rich_high = c(2, 3, 4, 5)
    )

  data_rich <-
    tibble::tibble(
      dataset_id = c(1, 1, 2, 2),
      age = c(0, 1000, 0, 1000),
      richness = c(1, 2, 3, 4),
      region = c("A", "A", "B", "B")
    )

  base::saveRDS(data_back, file = base::file.path(dir_data, "back_1.rds"))
  base::saveRDS(data_rich, file = base::file.path(dir_data, "rich_1.rds"))

  plot_iter <-
    plot_richness_iteration(
      data_back = base::file.path(dir_data, "back_1.rds"),
      data_rich = base::file.path(dir_data, "rich_1.rds"),
      iter = 1
    )

  testthat::expect_equal(plot_iter@data, data_back)
  testthat::expect_equal(plot_iter@layers[[1]][["data"]], data_rich)
})

testthat::test_that("plot_richness_iteration() selects the iteration", {
  dir_data <-
    withr::local_tempdir()

  data_back_1 <-
    tibble::tibble(
      region = "A",
      age = 0,
      richness = 1,
      rich_low = 0,
      rich_high = 2
    )

  data_back_2 <-
    tibble::tibble(
      region = "B",
      age = 500,
      richness = 7,
      rich_low = 6,
      rich_high = 8
    )

  data_rich_1 <-
    tibble::tibble(
      dataset_id = 1,
      age = 0,
      richness = 1,
      region = "A"
    )

  data_rich_2 <-
    tibble::tibble(
      dataset_id = 9,
      age = 500,
      richness = 7,
      region = "B"
    )

  base::saveRDS(data_back_1, file = base::file.path(dir_data, "back_1.rds"))
  base::saveRDS(data_back_2, file = base::file.path(dir_data, "back_2.rds"))
  base::saveRDS(data_rich_1, file = base::file.path(dir_data, "rich_1.rds"))
  base::saveRDS(data_rich_2, file = base::file.path(dir_data, "rich_2.rds"))

  vec_back <-
    base::file.path(dir_data, c("back_1.rds", "back_2.rds"))

  vec_rich <-
    base::file.path(dir_data, c("rich_1.rds", "rich_2.rds"))

  plot_iter <-
    plot_richness_iteration(
      data_back = vec_back,
      data_rich = vec_rich,
      iter = 2
    )

  testthat::expect_equal(plot_iter@data, data_back_2)
  testthat::expect_equal(plot_iter@layers[[1]][["data"]], data_rich_2)
})

testthat::test_that("plot_richness_iteration() accepts names and lists", {
  dir_data <-
    withr::local_tempdir()

  data_back <-
    tibble::tibble(
      region = "A",
      age = 0,
      richness = 1,
      rich_low = 0,
      rich_high = 2
    )

  data_rich <-
    tibble::tibble(
      dataset_id = 1,
      age = 0,
      richness = 1,
      region = "A"
    )

  base::saveRDS(data_back, file = base::file.path(dir_data, "back_5.rds"))
  base::saveRDS(data_rich, file = base::file.path(dir_data, "rich_5.rds"))

  vec_back <-
    c("5" = base::file.path(dir_data, "back_5.rds"))

  list_rich <-
    base::list("5" = base::file.path(dir_data, "rich_5.rds"))

  plot_iter <-
    plot_richness_iteration(
      data_back = vec_back,
      data_rich = list_rich,
      iter = "5"
    )

  testthat::expect_true(ggplot2::is_ggplot(plot_iter))
  testthat::expect_equal(plot_iter@data, data_back)
})

testthat::test_that("plot_richness_iteration() styles trend and ribbon", {
  dir_data <-
    withr::local_tempdir()

  data_back <-
    tibble::tibble(
      region = c("A", "A", "B", "B"),
      age = c(0, 1000, 0, 1000),
      richness = c(1, 2, 3, 4),
      rich_low = c(0, 1, 2, 3),
      rich_high = c(2, 3, 4, 5)
    )

  data_rich <-
    tibble::tibble(
      dataset_id = c(1, 1, 2, 2),
      age = c(0, 1000, 0, 1000),
      richness = c(1, 2, 3, 4),
      region = c("A", "A", "B", "B")
    )

  base::saveRDS(data_back, file = base::file.path(dir_data, "back_1.rds"))
  base::saveRDS(data_rich, file = base::file.path(dir_data, "rich_1.rds"))

  plot_iter <-
    plot_richness_iteration(
      data_back = base::file.path(dir_data, "back_1.rds"),
      data_rich = base::file.path(dir_data, "rich_1.rds"),
      iter = 1
    )

  data_sites <-
    ggplot2::get_layer_data(plot_iter, 1)

  data_mean <-
    ggplot2::get_layer_data(plot_iter, 2)

  data_ribbon <-
    ggplot2::get_layer_data(plot_iter, 3)

  testthat::expect_equal(base::unique(dplyr::pull(data_sites, linewidth)), 0.1)
  testthat::expect_equal(base::unique(dplyr::pull(data_sites, alpha)), 0.1)
  testthat::expect_equal(base::unique(dplyr::pull(data_mean, colour)), "red")
  testthat::expect_equal(base::unique(dplyr::pull(data_mean, linewidth)), 1)
  testthat::expect_equal(base::unique(dplyr::pull(data_ribbon, fill)), "blue")
  testthat::expect_equal(base::unique(dplyr::pull(data_ribbon, alpha)), 0.4)
})

testthat::test_that("plot_richness_iteration() maps values and reverses age", {
  dir_data <-
    withr::local_tempdir()

  data_back <-
    tibble::tibble(
      region = c("A", "A", "B", "B"),
      age = c(0, 1000, 0, 1000),
      richness = c(1, 2, 3, 4),
      rich_low = c(0, 1, 2, 3),
      rich_high = c(2, 3, 4, 5)
    )

  data_rich <-
    tibble::tibble(
      dataset_id = c(1, 1, 2, 2),
      age = c(0, 1000, 0, 1000),
      richness = c(1, 2, 3, 4),
      region = c("A", "A", "B", "B")
    )

  base::saveRDS(data_back, file = base::file.path(dir_data, "back_1.rds"))
  base::saveRDS(data_rich, file = base::file.path(dir_data, "rich_1.rds"))

  plot_iter <-
    plot_richness_iteration(
      data_back = base::file.path(dir_data, "back_1.rds"),
      data_rich = base::file.path(dir_data, "rich_1.rds"),
      iter = 1
    )

  data_mean <-
    ggplot2::get_layer_data(plot_iter, 2)

  data_ribbon <-
    ggplot2::get_layer_data(plot_iter, 3)

  testthat::expect_equal(
    base::sort(dplyr::pull(data_mean, x)),
    c(-1000, -1000, 0, 0)
  )
  testthat::expect_equal(
    base::sort(dplyr::pull(data_mean, y)),
    c(1, 2, 3, 4)
  )
  testthat::expect_equal(
    base::sort(dplyr::pull(data_ribbon, ymin)),
    c(0, 1, 2, 3)
  )
  testthat::expect_equal(
    base::sort(dplyr::pull(data_ribbon, ymax)),
    c(2, 3, 4, 5)
  )
  testthat::expect_equal(
    base::length(base::unique(dplyr::pull(data_mean, PANEL))),
    2L
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("plot_richness_iteration() handles a single region", {
  dir_data <-
    withr::local_tempdir()

  data_back <-
    tibble::tibble(
      region = c("A", "A"),
      age = c(0, 1000),
      richness = c(1, 2),
      rich_low = c(0, 1),
      rich_high = c(2, 3)
    )

  data_rich <-
    tibble::tibble(
      dataset_id = c(1, 1),
      age = c(0, 1000),
      richness = c(1, 2),
      region = c("A", "A")
    )

  base::saveRDS(data_back, file = base::file.path(dir_data, "back_1.rds"))
  base::saveRDS(data_rich, file = base::file.path(dir_data, "rich_1.rds"))

  plot_iter <-
    plot_richness_iteration(
      data_back = base::file.path(dir_data, "back_1.rds"),
      data_rich = base::file.path(dir_data, "rich_1.rds"),
      iter = 1
    )

  testthat::expect_equal(
    base::length(
      base::unique(dplyr::pull(ggplot2::get_layer_data(plot_iter, 2), PANEL))
    ),
    1L
  )
})


#----------------------------------------------------------#
# Side effects -----
#----------------------------------------------------------#

testthat::test_that("plot_richness_iteration() writes no files", {
  dir_data <-
    withr::local_tempdir()

  data_back <-
    tibble::tibble(
      region = "A",
      age = 0,
      richness = 1,
      rich_low = 0,
      rich_high = 2
    )

  data_rich <-
    tibble::tibble(
      dataset_id = 1,
      age = 0,
      richness = 1,
      region = "A"
    )

  base::saveRDS(data_back, file = base::file.path(dir_data, "back_1.rds"))
  base::saveRDS(data_rich, file = base::file.path(dir_data, "rich_1.rds"))

  vec_files_before <-
    base::list.files(dir_data)

  plot_iter <-
    plot_richness_iteration(
      data_back = base::file.path(dir_data, "back_1.rds"),
      data_rich = base::file.path(dir_data, "rich_1.rds"),
      iter = 1
    )

  testthat::expect_true(ggplot2::is_ggplot(plot_iter))
  testthat::expect_equal(base::list.files(dir_data), vec_files_before)
  testthat::expect_equal(
    base::readRDS(base::file.path(dir_data, "back_1.rds")),
    data_back
  )
})
