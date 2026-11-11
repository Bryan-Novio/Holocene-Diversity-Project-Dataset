get_pollen_counts_with_ages <- function(data_compilation) {    # function to obtain pollen counts with corresponding ages

  require(assertthat)
    
  assertthat::assert_that(
    is.data.frame(data_compilation),
    msg = "data_compilation has to be data.frame"
  )
  
  data_pollen <- get_pollen_counts(data_compilation)
  
  data_ages <- get_pollen_ages(data_compilation)
  
  inner_join(data_pollen, data_ages,
             by = c("dataset_id", 'sample_id'))
  
}