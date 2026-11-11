#' @title Prepare pollen data for richness estimation
#' @description
#' Standardises binned or non-binned pollen data into the long format that
#' [compute_richness()] expects, keeping only taxa with positive counts.
#' @param data_source
#' Data frame in long format. For `type = "binned"` it needs the columns
#' `dataset_id`, `BIN`, `taxa`, and `summed_pollen_count`. For
#' `type = "nonbinned"` it needs `dataset_id`, `age`, `taxa`, and
#' `pollen_counts`. `BIN` holds the bin number (as numeric, character, or a
#' factor).
#' @param type
#' Single character string, either `"binned"` (binned path) or
#' `"nonbinned"` (raw path).
#' @return
#' A tibble (or data frame, if `data_source` is a plain data frame) with the
#' columns `dataset_id`, `age`, `taxa`, and `pollen_grains`.
#' @details
#' In the binned path, `BIN` is renamed to `age` and converted to the bin
#' number times 1000 (a factor `BIN` is converted by its labels). The
#' multiplier of 1000 assumes bins of 1000 years. `summed_pollen_count` is
#' renamed to `pollen_grains`. In the raw path, `pollen_counts` is renamed to
#' `pollen_grains` and `age` is kept unchanged. Rows with `pollen_grains <= 0`
#' or `NA` are dropped in both paths, so a sample whose counts are all zero
#' disappears instead of getting a richness of 0. Fractional counts between
#' 0 and 1 are kept here but not counted as present by [compute_richness()].
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

  # Accept only the two known values, so a typo such as "Binned" does not
  #   silently select the raw path
  assertthat::assert_that(
    type %in% c("binned", "nonbinned"),
    msg = "type has to be either 'binned' or 'nonbinned'"
  )

  if (type == "binned") {
    
    assertthat::assert_that(
      all(
        c("BIN", "summed_pollen_count", "dataset_id", "taxa") %in%
          names(data_source)
      ),
      msg = "BIN and summed_pollen_count have to be columns in data_source"
    )
    
    res <- data_source %>% 
      rename(
        age = BIN,
        pollen_grains = summed_pollen_count
      ) %>% 
      select(dataset_id, age, taxa, pollen_grains) %>% 
      filter(pollen_grains > 0) %>%
      # Convert via character so that a factor BIN gives its labels, not
      #   its level codes
      mutate(age = as.numeric(as.character(age)) * 1000)
  
  } else {
    
    assertthat::assert_that(
      all(
        c("age", "pollen_counts", "dataset_id", "taxa") %in%
          names(data_source)
      ),
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
