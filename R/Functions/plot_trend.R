#' @title Plot a trend line with an interval ribbon
#' @description
#' Builds a plot of an estimated trend over time: a thick line for the
#' estimate and a semi-transparent ribbon for its interval, both in one
#' colour, with the y axis on the right and a blank panel background.
#' @param data
#' Data frame with the numeric columns `age` (x), `est` (estimate), `low`
#' (lower limit), and `upp` (upper limit).
#' @param color
#' Single character string. A colour name or hex code used for the line and
#' the ribbon.
#' @return
#' A `ggplot` object (not saved) with a `geom_line()` layer and a
#' `geom_ribbon()` layer. The x axis has breaks at 12000, 8000, 4000, and 0,
#' the y axis is on the right, and axis titles are removed.
#' @details
#' The colour value itself is not checked for being a valid colour; an
#' invalid colour fails only when the plot is drawn. The x axis is not
#' reversed, so `age` runs from low to high.
#' @seealso [plot_trend_with_reference_line()]
#' @examples
#' \dontrun{
#' plot_trend <-
#'   plot_trend(
#'     data = data_trend,
#'     color = "#1f77b4"
#'   )
#' }
plot_trend <- function(data, color) {
  assertthat::assert_that(
    is.data.frame(data),
    msg = "`data` must be a data frame."
  )

  assertthat::assert_that(
    all(c("age", "est", "low", "upp") %in% colnames(data)),
    msg = "`data` must contain columns 'age', 'est', 'low', and 'upp'."
  )

  assertthat::assert_that(
    length(color) == 1,
    is.character(color),
    is.na(color) == FALSE,
    msg = "`color` must be a single character string."
  )

  data %>% 
  ggplot(aes(x = age, y = est)) +
  geom_line(linewidth = 3, color = color) + 
  geom_ribbon(aes(ymin = low, 
                  ymax = upp),  fill = color, alpha = 0.1) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25, color = "black",angle = 90, vjust = 0.7),
        axis.text.y.right = element_text(size = 25, color = "black",angle = 90, vjust = 0.7, hjust = 0.5),
        axis.line.y.right = element_line(color = "black", linewidth = 1),
        axis.line.x.bottom = element_line(color = "black", linewidth = 1),
        axis.ticks.x = element_line(linewidth = 1),
        axis.ticks.y = element_line(linewidth = 1),
        axis.ticks.length.y.right = unit(.25, "cm"),
        axis.ticks.length.x.bottom = unit(.25, "cm"),
        panel.background = element_blank(),
  )  +
  scale_x_continuous(breaks = c(12000,8000, 4000, 0)) +
  scale_y_continuous(position = "right") 
}