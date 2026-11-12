#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("plot_trend() accepts valid inputs", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(1, 2, 3, 4),
      low = c(0.5, 1.5, 2.5, 3.5),
      upp = c(1.5, 2.5, 3.5, 4.5)
    )

  testthat::expect_no_error(
    plot_trend <-
      plot_trend(
        data = data_trend,
        color = "red"
      )
  )

  testthat::expect_true(ggplot2::is_ggplot(plot_trend))
})

testthat::test_that("plot_trend() validates data", {
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
      plot_trend(
        data = sel_bad,
        color = "red"
      ),
      "`data` must be a data frame\\."
    )
  }
})

testthat::test_that("plot_trend() validates required columns", {
  data_full <-
    tibble::tibble(
      age = c(0, 4000),
      est = c(1, 2),
      low = c(0.5, 1.5),
      upp = c(1.5, 2.5)
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
      plot_trend(
        data = data_missing,
        color = "red"
      ),
      "`data` must contain columns 'age', 'est', 'low', and 'upp'\\."
    )
  }
})

testthat::test_that("plot_trend() validates color", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000),
      est = c(1, 2),
      low = c(0.5, 1.5),
      upp = c(1.5, 2.5)
    )

  list_bad_color <-
    base::list(
      1,
      c("red", "blue"),
      NA_character_,
      NULL,
      character(0),
      TRUE
    )

  for (
    sel_bad in list_bad_color
  ) {
    testthat::expect_error(
      plot_trend(
        data = data_trend,
        color = sel_bad
      ),
      "`color` must be a single character string\\."
    )
  }
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("plot_trend() returns a line and a ribbon", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(1, 2, 3, 4),
      low = c(0.5, 1.5, 2.5, 3.5),
      upp = c(1.5, 2.5, 3.5, 4.5)
    )

  plot_trend <-
    plot_trend(
      data = data_trend,
      color = "red"
    )

  vec_geoms <-
    purrr::map_chr(
      .x = plot_trend@layers,
      .f = ~ base::class(.x[["geom"]])[1]
    )

  testthat::expect_true(ggplot2::is_ggplot(plot_trend))
  testthat::expect_equal(
    base::unname(vec_geoms),
    c("GeomLine", "GeomRibbon")
  )
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("plot_trend() applies the colour", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(1, 2, 3, 4),
      low = c(0.5, 1.5, 2.5, 3.5),
      upp = c(1.5, 2.5, 3.5, 4.5)
    )

  plot_trend <-
    plot_trend(
      data = data_trend,
      color = "#1F77B4"
    )

  data_line <-
    ggplot2::get_layer_data(plot_trend, 1)

  data_ribbon <-
    ggplot2::get_layer_data(plot_trend, 2)

  testthat::expect_equal(
    base::unique(dplyr::pull(data_line, colour)),
    "#1F77B4"
  )
  testthat::expect_equal(
    base::unique(dplyr::pull(data_ribbon, fill)),
    "#1F77B4"
  )
  testthat::expect_equal(base::unique(dplyr::pull(data_line, linewidth)), 3)
  testthat::expect_equal(base::unique(dplyr::pull(data_ribbon, alpha)), 0.1)
})

testthat::test_that("plot_trend() maps the estimate and interval", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(1, 2, 3, 4),
      low = c(0.5, 1.5, 2.5, 3.5),
      upp = c(1.5, 2.5, 3.5, 4.5)
    )

  plot_trend <-
    plot_trend(
      data = data_trend,
      color = "red"
    )

  data_line <-
    ggplot2::get_layer_data(plot_trend, 1)

  data_ribbon <-
    ggplot2::get_layer_data(plot_trend, 2)

  testthat::expect_equal(dplyr::pull(data_line, x), c(0, 4000, 8000, 12000))
  testthat::expect_equal(dplyr::pull(data_line, y), c(1, 2, 3, 4))
  testthat::expect_equal(dplyr::pull(data_ribbon, ymin), c(0.5, 1.5, 2.5, 3.5))
  testthat::expect_equal(dplyr::pull(data_ribbon, ymax), c(1.5, 2.5, 3.5, 4.5))
})

testthat::test_that("plot_trend() sets x breaks and y axis side", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(1, 2, 3, 4),
      low = c(0.5, 1.5, 2.5, 3.5),
      upp = c(1.5, 2.5, 3.5, 4.5)
    )

  plot_trend <-
    plot_trend(
      data = data_trend,
      color = "red"
    )

  list_built <-
    ggplot2::ggplot_build(plot_trend)

  vec_breaks <-
    list_built@layout[["panel_params"]][[1]][["x"]][["breaks"]]

  scale_y <-
    plot_trend@scales[["get_scales"]]("y")

  testthat::expect_equal(vec_breaks, c(12000, 8000, 4000, 0))
  testthat::expect_equal(scale_y[["position"]], "right")
})

testthat::test_that("plot_trend() removes the axis titles", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000),
      est = c(1, 2),
      low = c(0.5, 1.5),
      upp = c(1.5, 2.5)
    )

  plot_trend <-
    plot_trend(
      data = data_trend,
      color = "red"
    )

  testthat::expect_s3_class(
    plot_trend@theme[["axis.title.x"]],
    "ggplot2::element_blank"
  )
  testthat::expect_s3_class(
    plot_trend@theme[["axis.title.y"]],
    "ggplot2::element_blank"
  )
})

testthat::test_that("plot_trend() builds without warnings", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000, 8000, 12000),
      est = c(1, 2, 3, 4),
      low = c(0.5, 1.5, 2.5, 3.5),
      upp = c(1.5, 2.5, 3.5, 4.5)
    )

  plot_trend <-
    plot_trend(
      data = data_trend,
      color = "red"
    )

  testthat::expect_no_warning(
    ggplot2::ggplot_build(plot_trend)
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("plot_trend() ignores extra columns", {
  data_trend <-
    tibble::tibble(
      age = c(0, 4000),
      est = c(1, 2),
      low = c(0.5, 1.5),
      upp = c(1.5, 2.5),
      note = c("a", "b")
    )

  plot_trend <-
    plot_trend(
      data = data_trend,
      color = "blue"
    )

  testthat::expect_equal(
    dplyr::pull(ggplot2::get_layer_data(plot_trend, 1), y),
    c(1, 2)
  )
})

testthat::test_that("plot_trend() handles a single row", {
  data_trend <-
    tibble::tibble(
      age = 4000,
      est = 2,
      low = 1,
      upp = 3
    )

  plot_trend <-
    plot_trend(
      data = data_trend,
      color = "red"
    )

  testthat::expect_true(ggplot2::is_ggplot(plot_trend))
  testthat::expect_equal(
    base::nrow(ggplot2::get_layer_data(plot_trend, 2)),
    1L
  )
})
