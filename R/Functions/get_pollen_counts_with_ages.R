get_pollen_counts_with_ages <- function(data_compilation) {    # function to obtain pollen counts with corresponding ages
  data_pollen <- get_pollen_counts(data_compilation)
  
  data_ages <- get_pollen_ages(data_compilation)
  
  inner_join(data_pollen, data_ages,
             by = c("dataset_id", 'sample_id'))
  
}