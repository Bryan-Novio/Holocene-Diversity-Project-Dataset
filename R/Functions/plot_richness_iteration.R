#' @title Plot richness trends of one iteration with site-level trends
#' @description
#' Builds a plot of the back-transformed richness of a single model iteration
#' per region: the mean trend (red line) with its interval (blue ribbon),
#' drawn over the thin richness trends of the individual datasets.
#' @param data_back
#' Character vector, or list, of paths to `.rds` files with back-transformed
#' predictions (one file per iteration, optionally named by iteration). The
#' selected file must hold a data frame with the columns `region`, `age`,
#' `richness`, `rich_low`, and `rich_high`.
#' @param data_rich
#' Character vector, or list, of paths to `.rds` files with dataset-level
#' richness (one file per iteration, in the same order as `data_back`). The
#' selected file must hold a data frame with the columns `dataset_id`, `age`,
#' `richness`, and `region`.
#' @param iter
#' A single whole number between 1 and the length of `data_back` and
#' `data_rich` (an index), or a single string matching a name of both.
#' @return
#' A `ggplot` object (not saved), with one panel per `region` via
#' [ggplot2::facet_wrap()]: thin transparent lines per `dataset_id`, a red
#' mean line, a blue ribbon between `rich_low` and `rich_high`, a classic
#' theme, and a reversed x axis (`age`).
#' @details
#' Only the existence of the selected files is checked; the required columns
#' are not validated, so missing columns surface as a ggplot error when the
#' plot is built. Both files are read with [readr::read_rds()].
#' @seealso [load_predicted_richness()]
#' @examples
#' \dontrun{
#' plot_richness <-
#'   plot_richness_iteration(
#'     data_back = vec_files_back,
#'     data_rich = vec_files_rich,
#'     iter = 1
#'   )
#' }
plot_richness_iteration <- function(data_back,data_rich,iter) {
  assertthat::assert_that(
    is.character(data_back) || is.list(data_back),
    msg = "`data_back` must be a character vector or a list of file paths."
  )

  assertthat::assert_that(
    is.character(data_rich) || is.list(data_rich),
    msg = "`data_rich` must be a character vector or a list of file paths."
  )

  assertthat::assert_that(
    length(iter) == 1,
    is.numeric(iter) || is.character(iter),
    is.na(iter) == FALSE,
    msg = "`iter` must be a single number or string."
  )

  assertthat::assert_that(
    (is.numeric(iter) && iter == round(iter) &&
      iter >= 1 && iter <= min(length(data_back), length(data_rich))) ||
      (is.character(iter) &&
        iter %in% names(data_back) && iter %in% names(data_rich)),
    msg = "`iter` must be an index or a name present in both inputs."
  )

  assertthat::assert_that(
    file.exists(data_back[[iter]]),
    msg = "The selected file of `data_back` does not exist."
  )

  assertthat::assert_that(
    file.exists(data_rich[[iter]]),
    msg = "The selected file of `data_rich` does not exist."
  )

  data_back[[iter]] %>%
    readr::read_rds() %>%
    ggplot(aes(x = age, y = richness)) +
    geom_line(data = readr::read_rds(data_rich[[iter]]), aes(group = dataset_id), linewidth = 0.1, alpha = 0.1) +
    geom_line(linewidth = 1, color= "red") +
    geom_ribbon(aes(ymin = rich_low, ymax = rich_high, y = NULL), fill = "blue", alpha = 0.4) +
    facet_wrap(~ region) +
    theme_classic() +
    scale_x_reverse()
}
