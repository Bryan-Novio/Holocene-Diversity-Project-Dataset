#' @title Load back-transformed richness predictions of one iteration
#' @description
#' Reads the back-transformed richness predictions of a single iteration of
#' the study 3 model from an `.rds` file and keeps only the columns `region`,
#' `age`, and `richness`.
#' @param iteration
#' A single number or string. The file name, without extension, of the
#' iteration to load (e.g. `1` for `<path>/1.rds`).
#' @param path
#' Single character string. Directory holding the `.rds` files. Defaults to
#' `Data/Paper_1/data_model/data_back_new/` in the project root, found with
#' [here::here()].
#' @return
#' A data frame (the class stored in the file, typically a tibble) with the
#' columns `region`, `age`, and `richness`, in this order.
#' @details
#' The file `<path>/<iteration>.rds` must exist and hold a data frame with at
#' least the columns `region`, `age`, and `richness`. Any other columns, such
#' as `rich_low` and `rich_high`, are dropped. The function is meant to be
#' mapped over iterations, e.g. with [purrr::map()].
#' @seealso [plot_richness_iteration()]
#' @examples
#' \dontrun{
#' data_iteration_1 <-
#'   load_predicted_richness(iteration = 1)
#'
#' list_iterations <-
#'   purrr::map(
#'     .x = 1:10,
#'     .f = load_predicted_richness
#'   )
#' }
load_predicted_richness <- function(
  iteration,
  path = here::here("Data/Paper_1/data_model/data_back_new/")
) {
  assertthat::assert_that(
    length(iteration) == 1,
    is.numeric(iteration) || is.character(iteration),
    is.na(iteration) == FALSE,
    msg = "`iteration` must be a single number or string."
  )

  assertthat::assert_that(
    length(path) == 1,
    is.character(path),
    is.na(path) == FALSE,
    msg = "`path` must be a single character string."
  )

  file_iteration <- paste0(path, "/", iteration, ".rds")

  assertthat::assert_that(
    file.exists(file_iteration),
    msg = "The file '<path>/<iteration>.rds' does not exist."
  )

  data_iteration <-
    file_iteration %>%
    readr::read_rds()

  assertthat::assert_that(
    is.data.frame(data_iteration),
    all(c("region", "age", "richness") %in% colnames(data_iteration)),
    msg = "The file must hold a data frame with 'region', 'age', 'richness'."
  )

  res <-
    data_iteration %>%
    dplyr::select(region, age, richness)

  return(res)
}
