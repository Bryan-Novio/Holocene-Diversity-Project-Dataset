#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("plot_trend_with_reference_line() accepts valid inputs", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(7, 8, 9, 10),
      low = c(6.5, 7.5, 8.5, 9.5),
      upp = c(7.5, 8.5, 9.5, 10.5)
    )

  testthat::expect_no_error(
    plot_trend <-
      plot_trend_with_reference_line(data = data_trend)
  )

  testthat::expect_true(ggplot2::is_ggplot(plot_trend))
})

testthat::test_that("plot_trend_with_reference_line() validates data", {
  list_bad_data <-
    base::list(
      NULL,
      1:3,
      "data_trend",
      base::list(age = 1, est = 1, low = 1, upp = 1)
    )

  for (
    sel_bad in list_bad_data
  ) {
    testthat::expect_error(
      plot_trend_with_reference_line(data = sel_bad),
      "`data` must be a data frame\\."
    )
  }
})

testthat::test_that("plot_trend_with_reference_line() checks columns", {
  data_full <-
    tibble::tibble(
      age = c(0, 4000),
      est = c(7, 8),
      low = c(6.5, 7.5),
      upp = c(7.5, 8.5)
    )

  vec_required <-
    c("age", "est", "low", "upp")

  for (
    sel_col in vec_required
  ) {
    data_missing <-
      dplyr::select(
        data_full,
        -dplyr::all_of(sel_col)
      )

    testthat::expect_error(
      plot_trend_with_reference_line(data = data_missing),
      "`data` must contain columns 'age', 'est', 'low', and 'upp'\\."
    )
  }
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("plot_trend_with_reference_line() returns 3 layers", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(7, 8, 9, 10),
      low = c(6.5, 7.5, 8.5, 9.5),
      upp = c(7.5, 8.5, 9.5, 10.5)
    )

  plot_trend <-
    plot_trend_with_reference_line(data = data_trend)

  vec_geoms <-
    purrr::map_chr(
      .x = plot_trend@layers,
      .f = ~ base::class(.x[["geom"]])[1]
    )

  testthat::expect_true(ggplot2::is_ggplot(plot_trend))
  testthat::expect_equal(
    base::unname(vec_geoms),
    c("GeomLine", "GeomRibbon", "GeomVline")
  )
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("plot_trend_with_reference_line() draws vline at 9500", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(7, 8, 9, 10),
      low = c(6.5, 7.5, 8.5, 9.5),
      upp = c(7.5, 8.5, 9.5, 10.5)
    )

  plot_trend <-
    plot_trend_with_reference_line(data = data_trend)

  data_vline <-
    ggplot2::get_layer_data(plot_trend, 3)

  testthat::expect_equal(dplyr::pull(data_vline, xintercept), 9500)
  testthat::expect_equal(dplyr::pull(data_vline, colour), "red")
  testthat::expect_equal(dplyr::pull(data_vline, linewidth), 2)
})

testthat::test_that("plot_trend_with_reference_line() maps est, low, upp", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(7, 8, 9, 10),
      low = c(6.5, 7.5, 8.5, 9.5),
      upp = c(7.5, 8.5, 9.5, 10.5)
    )

  plot_trend <-
    plot_trend_with_reference_line(data = data_trend)

  data_line <-
    ggplot2::get_layer_data(plot_trend, 1)

  data_ribbon <-
    ggplot2::get_layer_data(plot_trend, 2)

  testthat::expect_equal(dplyr::pull(data_line, x), c(0, 4000, 8000, 12000))
  testthat::expect_equal(dplyr::pull(data_line, y), c(7, 8, 9, 10))
  testthat::expect_equal(dplyr::pull(data_line, colour), base::rep("black", 4))
  testthat::expect_equal(dplyr::pull(data_ribbon, ymin), c(6.5, 7.5, 8.5, 9.5))
  testthat::expect_equal(dplyr::pull(data_ribbon, ymax), c(7.5, 8.5, 9.5, 10.5))
})

testthat::test_that("plot_trend_with_reference_line() limits y to 6-14", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(7, 8, 9, 30),
      low = c(6.5, 7.5, 8.5, 25),
      upp = c(7.5, 8.5, 9.5, 35)
    )

  plot_trend <-
    plot_trend_with_reference_line(data = data_trend)

  scale_y <-
    plot_trend@scales[["get_scales"]]("y")

  testthat::expect_s3_class(plot_trend@coordinates, "CoordCartesian")
  testthat::expect_equal(plot_trend@coordinates[["limits"]][["y"]], c(6, 14))
  testthat::expect_equal(scale_y[["position"]], "right")
  testthat::expect_equal(
    dplyr::pull(ggplot2::get_layer_data(plot_trend, 1), y),
    c(7, 8, 9, 30)
  )
})

testthat::test_that("plot_trend_with_reference_line() builds silently", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(7, 8, 9, 10),
      low = c(6.5, 7.5, 8.5, 9.5),
      upp = c(7.5, 8.5, 9.5, 10.5)
    )

  testthat::expect_no_warning(
    ggplot2::ggplot_build(
      plot_trend_with_reference_line(data = data_trend)
    )
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("plot_trend_with_reference_line() ignores extra columns", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000),
      est = c(7, 8),
      low = c(6.5, 7.5),
      upp = c(7.5, 8.5),
      note = c("a", "b")
    )

  plot_trend <-
    plot_trend_with_reference_line(data = data_trend)

  testthat::expect_equal(
    dplyr::pull(ggplot2::get_layer_data(plot_trend, 1), y),
    c(7, 8)
  )
})

testthat::test_that("plot_trend_with_reference_line() handles a single row", {
  data_trend <-
    tibble::tibble(
      age = 4000,
      est = 8,
      low = 7,
      upp = 9
    )

  plot_trend <-
    plot_trend_with_reference_line(data = data_trend)

  testthat::expect_true(ggplot2::is_ggplot(plot_trend))
  testthat::expect_equal(
    base::nrow(ggplot2::get_layer_data(plot_trend, 2)),
    1L
  )
})
