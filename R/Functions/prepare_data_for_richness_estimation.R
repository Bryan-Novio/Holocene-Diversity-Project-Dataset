prepare_data_for_richness_estimation <- function(data_source, type){
  
  assertthat::assert_that(
  is.data.frame(data_source),
  msg = "data_source has to be a data.frame"
  )
  
  if (type == "binned"){
    res <-
    prepared_data_for_richness_estimation <- data_source %>% 
      rename(
        age = BIN,
        pollen_grains = summed_pollen_count
      ) %>% 
      select(dataset_id,age, taxa, pollen_grains) %>% 
      filter(pollen_grains > 0) %>% 
      mutate(age = as.numeric(age)  * 500) 
  } else {
    res <-
    prepared_data_for_richness_estimation <- data_source %>% 
      rename(
        pollen_grains = pollen_counts
      ) %>% 
      select(dataset_id,age, taxa, pollen_grains) %>% 
      filter(pollen_grains > 0) 
  }
  
  return(res)
  
}
