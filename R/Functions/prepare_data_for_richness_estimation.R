#' @title Prepare pollen data for richness estimation
#' @description
#' Standardises binned or non-binned pollen data into the long format that
#' [compute_richness()] expects, keeping only taxa with positive counts.
#' @param data_source
#' Data frame in long format. For `type = "binned"` it needs the columns
#' `dataset_id`, `BIN`, `taxa`, and `summed_pollen_count`. For any other
#' `type` it needs `dataset_id`, `age`, `taxa`, and `pollen_counts`. `BIN`
#' holds the bin number (as numeric, character, or a factor).
#' @param type
#' Single non-missing character string. `"binned"` selects the binned path;
#' every other value (callers use `"nonbinned"`) selects the raw path.
#' @return
#' A tibble with the columns `dataset_id`, `age`, `taxa`, and `pollen_grains`.
#' @details
#' In the binned path, `BIN` is renamed to `age` and converted with
#' `as.numeric(BIN) * 1000`, so the multiplier of 1000 assumes bins of 1000
#' years and `BIN` giving the bin number. `summed_pollen_count` is renamed to
#' `pollen_grains`. In the raw path, `pollen_counts` is renamed to
#' `pollen_grains` and `age` is kept unchanged. Rows with `pollen_grains <= 0`
#' or `NA` are dropped in both paths. Known gap: [bin_pollen_counts()]
#' returns `BIN` as a factor whose labels are integers, and `as.numeric()` of
#' a factor gives the level codes, not the labels, so ages are wrong unless
#' the labels equal the level positions (1, 2, 3, ...).
#' @seealso [compute_richness()], [bin_pollen_counts()]
#' @examples
#' \dontrun{
#' data_richness_input <-
#'   prepare_data_for_richness_estimation(
#'     data_source = data_binned,
#'     type = "binned"
#'   )
#' }
prepare_data_for_richness_estimation <- function(data_source, type){

  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_source has to be a data.frame"
  )

  assertthat::assert_that(
    is.character(type),
    length(type) == 1,
    !is.na(type),
    msg = "type has to be a single non-NA character string"
  )
  
  if (type == "binned") {
    
    assertthat::assert_that(
      all(c("BIN", "summed_pollen_count", "dataset_id", "taxa") %in% names(data_source)),
      msg = "BIN and summed_pollen_count have to be columns in data_source"
    )
    
    res <- data_source %>% 
      rename(
        age = BIN,
        pollen_grains = summed_pollen_count
      ) %>% 
      select(dataset_id, age, taxa, pollen_grains) %>% 
      filter(pollen_grains > 0) %>% 
      mutate(age = as.numeric(age) * 1000)
  
  } else {
    
    assertthat::assert_that(
      all(c("age", "pollen_counts", "dataset_id", "taxa") %in% names(data_source)),
      msg = "age and pollen_counts have to be columns in data_source"
    )
    
    res <- data_source %>% 
      rename(
        pollen_grains = pollen_counts
      ) %>% 
      select(dataset_id, age, taxa, pollen_grains) %>% 
      filter(pollen_grains > 0)
  }
  
  return(res)
}
