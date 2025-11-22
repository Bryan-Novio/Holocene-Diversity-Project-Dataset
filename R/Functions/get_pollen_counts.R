get_pollen_counts <- function(data_compilation) {   # function to obtain pollen counts
  
  assert_that(
    is.data.frame(data_compilation),
    all(c("dataset_id", "raw_counts") %in% colnames(data_compilation)),
    msg = "Input data_compilation must be a data frame containing 'dataset_id' and 'raw_counts' columns"
  )
  
  res <-
    data_compilation %>% 
    select(dataset_id, raw_counts) %>% 
    unnest(raw_counts) %>% 
    pivot_longer(
      cols = !c(dataset_id,sample_id),
      names_to = "taxa", values_to = "pollen_counts",
      values_drop_na = TRUE)
  
  return(res)
}