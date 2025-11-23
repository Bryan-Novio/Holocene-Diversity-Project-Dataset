bin_data <- function(data_source, binning_var, bin_size){
  
  # Assert data_source is a data.frame
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_source has to be a data.frame"
  )
  
  # Assert bin_size is numeric and positive
  assertthat::assert_that(
    is.numeric(bin_size) && length(bin_size) == 1 && bin_size > 0,
    msg = "bin_size must be a single positive numeric value"
  )
  
  data_binned <- data_source %>% 
    mutate(
      BIN = cut(age, seq(min(age), max(age) + bin_size, bin_size), right = FALSE),
      BIN_chr = as.character(BIN),
      BIN_fct = as.factor(BIN_chr),
      BIN_int = as.factor(as.numeric(BIN_fct)), # recode bins to integer, then factor
      BIN = BIN_int
    ) %>% 
    group_by({{binning_var}}, taxa, BIN, BIN_chr) %>% 
    summarise(summed_pollen_count = sum(pollen_counts), .groups = "drop")
  
  return(data_binned)
}
