#' @title Rarefy all samples repeatedly and save each iteration
#' @description
#' Runs [rarefy_all_samples()] `n_iter` times and saves the result of
#' iteration `i` as `<path>/<i>.rds`. Iterations whose file already exists and
#' is not empty are skipped, so an interrupted run can be resumed.
#' @param data_to_rarefy
#' Data frame in wide format as required by [rarefy_all_samples()]: the
#' columns `dataset_id`, `age`, and one count column per taxon.
#' @param n_iter
#' Single positive whole number. Number of rarefaction iterations (files).
#' Default is 1000.
#' @param n_grains
#' Single positive whole number. Number of grains per sample, passed to
#' [rarefy_all_samples()]. Default is 300.
#' @param path
#' Single character string. Path of an existing directory where the `.rds`
#' files are written.
#' @return
#' Invisibly, a character vector of length `n_iter` with the file paths
#' `<path>/1.rds` to `<path>/<n_iter>.rds`, whether they were written now or
#' skipped.
#' @details
#' Each file holds the tibble returned by [rarefy_all_samples()]. The random
#' number generator is not reset, so set a seed before calling for
#' reproducibility. A skipped file is neither read nor checked for content, so
#' stale files from a run with different settings are kept.
#' @seealso [rarefy_all_samples()]
#' @examples
#' \dontrun{
#' set.seed(900723)
#' vec_files <-
#'   save_rarefied_iterations(
#'     data_to_rarefy = data_pollen_wide,
#'     n_iter = 100,
#'     n_grains = 300,
#'     path = here::here("Data/rarefied")
#'   )
#' }
save_rarefied_iterations <- function(
  data_to_rarefy,
  n_iter = 1000,
  n_grains = 300,
  path
) {
  
  assertthat::assert_that(
    is.data.frame(data_to_rarefy),
    msg = "data_to_rarefy has to be a data.frame"
  )
  
  assertthat::assert_that(
    is.numeric(n_iter),
    length(n_iter) == 1,
    !is.na(n_iter),
    n_iter >= 1,
    n_iter == round(n_iter),
    msg = "n_iter must be a single positive whole number"
  )
  
  assertthat::assert_that(
    is.numeric(n_grains),
    length(n_grains) == 1,
    is.finite(n_grains),
    n_grains >= 1,
    n_grains == round(n_grains),
    msg = "n_grains must be a single positive whole number"
  )
  
  assertthat::assert_that(
    is.character(path),
    length(path) == 1,
    !is.na(path),
    dir.exists(path),
    msg = "path must be a single existing directory"
  )
  
  1:n_iter %>% 
    purrr::set_names() %>% 
    purrr::walk(
      .progress = TRUE,
      .f = function(x) {
        file_path <- file.path(path, paste0(x, ".rds"))
        
        # SKIP LOGIC: If file exists and is not empty, skip to next iteration
        if (file.exists(file_path) && file.info(file_path)$size > 0) {
          return(NULL)
            
        }

        
        # Otherwise, run rarefaction and save
        rarefy_all_samples(data_to_rarefy, n_grains = n_grains) %>% 
          readr::write_rds(file = file_path)
      }
    )
  
  res <-
    file.path(
      path,
      as.character(stringr::str_glue("{seq_len(n_iter)}.rds"))
    )
  
  return(invisible(res))
}
