bin_data <- function(data_source, bin_size){
  
  data_binned <-  data_source %>% 
    mutate(
      BIN = cut(age, seq(min(age), 
                         max(age) + bin_size, bin_size), right = FALSE),
      BIN_chr = as.character(BIN),
      BIN_fct = as.factor(BIN_chr),
      BIN_int = as.factor(as.numeric(BIN_fct)), # recode BINS to integer, then factor) 
      BIN = BIN_int) %>% 
    group_by(dataset_id , taxa, BIN, BIN_chr) %>% 
    summarise(summed_pollen_count = sum(pollen_counts), .groups = "drop")
  
  return(data_binned)
}