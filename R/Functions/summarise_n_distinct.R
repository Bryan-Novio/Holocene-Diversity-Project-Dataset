#' @title Count distinct datasets, samples, or taxa
#' @description
#' Counts the distinct values of one metric column (`dataset_id`,
#' `sample_id`, or `taxa`), optionally within groups, and optionally labels
#' the result with a name.
#' @param data_source
#' Data frame with the column named by `metric` (and the columns named in
#' `group_var`).
#' @param metric
#' One of `"dataset_id"` (default), `"sample_id"`, or `"taxa"`. Partial
#' matching is allowed, see [base::match.arg()].
#' @param group_var
#' `NULL` (default) or a character vector of column names of `data_source`
#' to count within. `NULL` gives a single overall count.
#' @param name
#' `NULL` (default) or a single character string. If given, it is added as
#' the column `data` to the result.
#' @return
#' An ungrouped tibble with the grouping columns (if `group_var` is given),
#' the column `n` (integer, number of distinct values of `metric`), and the
#' column `data` (if `name` is given).
#' @details
#' `NA` values of the metric column are not counted (`na.rm = TRUE`), unlike
#' in [summarise_n_datasets()], [summarise_n_samples()], and
#' [summarise_n_taxa()]. Groups that do not occur in the data are not
#' returned.
#' @seealso [summarise_n_datasets()], [summarise_n_samples()],
#' [summarise_n_taxa()]
#' @examples
#' \dontrun{
#' table_n <-
#'   summarise_n_distinct(
#'     data_source = data_pollen,
#'     metric = "taxa",
#'     group_var = c("region", "climatezone"),
#'     name = "all"
#'   )
#' }
summarise_n_distinct <- function(data_source, 
                     metric = c("dataset_id", "sample_id", "taxa"),
                     group_var = NULL, 
                     name = NULL) {
 
 # Validate inputs
assertthat::assert_that(
  is.data.frame(data_source),
  msg = "`data_source` must be a data frame."
)
  
       metric <- match.arg(metric)
       
         assertthat::assert_that(
             metric %in% names(data_source),
             msg = "The `metric` column must exist in `data_source`."
           )
       
         if (!is.null(group_var)) {
             assertthat::assert_that(
                 is.character(group_var),
                 msg = "`group_var` must be NULL or a character vector."
               )
             assertthat::assert_that(
                 all(group_var %in% names(data_source)),
                 msg = "All specified group_var columns must exist in data_source."
               )
             data_source <- data_source %>% 
                 dplyr::group_by(dplyr::pick(dplyr::all_of(group_var)))
           }
       
         # Calculate unique counts for the selected metric
         data_count <- data_source %>% 
             dplyr::summarise(
                 n = dplyr::n_distinct(.data[[metric]], na.rm = TRUE),
                 .groups = "drop"
               )
         
           # Add label column if 'name' is provided
           if (!is.null(name)) {
               assertthat::assert_that(
                 is.character(name),
                 length(name) == 1,
                 !is.na(name),
                 msg = "`name` must be NULL or a single character string."
               )
               res <- data_count %>% 
                   dplyr::mutate(data = as.character(name))
             } else {
                 res <- data_count
               }
         
           return(res)
       }