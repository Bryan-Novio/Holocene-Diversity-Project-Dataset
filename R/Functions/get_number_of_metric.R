get_number_of_metric <- function(data_source, 
                     metric = c("dataset_id", "sample_id", "taxa"),
                     group_var = NULL, 
                     name = NULL) {
 
 # Validate inputs
assertthat::assert_that(is.data.frame(data_source))
  
       metric <- match.arg(metric)
       
         assertthat::assert_that(
             metric %in% names(data_source),
             msg = paste0("Column '", metric, "' not found in data_source.")
           )
       
         if (!is.null(group_var)) {
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
               assertthat::assert_that(is.character(name))
               res <- data_count %>% 
                   dplyr::mutate(data = as.character(name))
             } else {
                 res <- data_count
               }
         
           return(res)
       }