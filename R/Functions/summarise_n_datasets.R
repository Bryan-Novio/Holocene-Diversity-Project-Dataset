#' @title Count the number of datasets
#' @description
#' Counts the distinct values of `dataset_id` in `data_source`, optionally
#' within groups, and optionally labels the result with a name.
#' @param data_source
#' Data frame with the column `dataset_id` (and the columns named in
#' `group_var`).
#' @param group_var
#' `NULL` (default) or a character vector of column names of `data_source`
#' to count within. `NULL` gives a single overall count.
#' @param name
#' `NULL` (default) or a single character string. If given, it is added as
#' the column `data` to the result.
#' @return
#' A tibble with the grouping columns (if `group_var` is given; they keep
#' their own names), the column `n` (integer, number of distinct `dataset_id`),
#' and the column `data` (if `name` is given).
#' @details
#' `NA` values of `dataset_id` are counted as one distinct value, unlike in
#' [summarise_n_distinct()], which drops them. Groups that do not occur in
#' the data are not returned. When `group_var` is given, the result is
#' still grouped by those columns (a grouped tibble), unlike the ungrouped
#' result of [summarise_n_distinct()].
#' @seealso [summarise_n_samples()], [summarise_n_taxa()],
#' [summarise_n_distinct()]
#' @examples
#' \dontrun{
#' table_n <-
#'   summarise_n_datasets(
#'     data_source = data_pollen,
#'     group_var = "region",
#'     name = "all"
#'   )
#' }
summarise_n_datasets <- function(data_source, group_var = NULL, name = NULL) {
  
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
  )
  
  assertthat::assert_that(
    "dataset_id" %in% names(data_source),
    msg = "`data_source` must contain the column 'dataset_id'."
  )
  
  
  if (
    !is.null(group_var)
  ) {
    assertthat::assert_that(
      is.character(group_var),
      msg = "`group_var` must be NULL or a character vector."
    )

    assertthat::assert_that(
      all(group_var %in% names(data_source)),
      msg = "All `group_var` columns must exist in `data_source`."
    )
    
    data_source <- 
      data_source %>% 
      dplyr::group_by(dplyr::across(dplyr::all_of(group_var)))
  }
  
  
  data_count <- 
    data_source %>% 
    distinct(dataset_id) %>% 
    dplyr::count() 
  
  if(
    isFALSE(is.null(name))
  ) {
    
    assertthat::assert_that(
      is.character(name),
      length(name) == 1,
      !is.na(name),
      msg = "`name` must be NULL or a single character string."
    )
    
    res <- 
      data_count %>% 
      mutate(data = as.character(name))
      
    
  } else {
    res <- 
      data_count 
  }
  
  
  return(res)
}

