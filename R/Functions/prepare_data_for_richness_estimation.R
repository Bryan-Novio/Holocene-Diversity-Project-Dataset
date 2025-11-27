prepare_data_for_richness_estimation <- function(data_source, type){
  
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_source has to be a data.frame"
  )
  
  if (type == "binned") {
    
    assertthat::assert_that(
      all(c("BIN", "summed_pollen_count", "dataset_id", "taxa") %in% names(data_source)),
      msg = "BIN and summed_pollen_count have to be columns in data_source"
    )
    
    res <- data_source %>% 
      rename(
        age = BIN,
        pollen_grains = summed_pollen_count
      ) %>% 
      select(dataset_id, age, taxa, pollen_grains) %>% 
      filter(pollen_grains > 0) %>% 
      mutate(age = as.numeric(age) * 1000)
    
  } else {
    
    assertthat::assert_that(
      all(c("age", "pollen_counts", "dataset_id", "taxa") %in% names(data_source)),
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
