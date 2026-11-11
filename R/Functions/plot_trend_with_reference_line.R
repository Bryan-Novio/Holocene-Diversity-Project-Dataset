#' @title Plot the trend of study 2 with a threshold line
#' @description
#' Builds a plot of an estimated trend over time for study 2: a thick black
#' line for the estimate, a grey ribbon for its interval, and a red vertical
#' line at age 9500.
#' @param data
#' Data frame with the numeric columns `age` (x), `est` (estimate), `low`
#' (lower limit), and `upp` (upper limit).
#' @return
#' A `ggplot` object (not saved) with a `geom_line()`, a `geom_ribbon()`, and
#' a `geom_vline()` layer at `xintercept = 9500`. The y axis is on the right
#' and zoomed to 6 to 14 with `coord_cartesian()` (data outside the range are
#' not removed), and axis titles are removed.
#' @details
#' The vertical line position (9500) and the y range (6 to 14) are fixed in
#' the function and cannot be changed through arguments, so the plot suits
#' estimates on the scale of study 2 only.
#' @seealso [plot_trend()]
#' @examples
#' \dontrun{
#' plot_trend <-
#'   plot_trend_with_reference_line(data = data_trend)
#' }
plot_trend_with_reference_line <- function(data) {
  assertthat::assert_that(
    is.data.frame(data),
    msg = "`data` must be a data frame."
  )

  assertthat::assert_that(
    all(c("age", "est", "low", "upp") %in% colnames(data)),
    msg = "`data` must contain columns 'age', 'est', 'low', and 'upp'."
  )

  
  data %>% 
  ggplot2::ggplot(aes(x = age, y = est))+ 
    ggplot2::geom_line(color = "black", linewidth = 3
    ) +
    geom_ribbon(aes(ymin = low, ymax = upp), colour = "gray",alpha = 0.5
    ) +
    ggplot2::theme(axis.text.x = element_text(size = 25, color = "black",angle = 90, vjust = 0.7),
                   axis.text.y.right = element_text(size = 25, color = "black",angle = 90, vjust = 0.7, hjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   panel.background = element_blank(),
                   axis.line.y.right  = element_line(color = "black", linewidth = 1),
                   axis.line.x.bottom = element_line(color = "black", linewidth = 1),
                   axis.ticks.x = element_line(linewidth = 1),
                   axis.ticks.y = element_line(linewidth = 1),
                   axis.ticks.length.y.right = unit(.25, "cm"),
                   axis.ticks.length.x.bottom = unit(.25, "cm"))  +
    ggplot2::coord_cartesian(
      ylim = c(6,14)
    ) +
    ggplot2::scale_x_continuous() +   
    scale_y_continuous(position = "right") +
    ggplot2::geom_vline(xintercept = 9500, color ="red", linewidth = 2)
  
}

