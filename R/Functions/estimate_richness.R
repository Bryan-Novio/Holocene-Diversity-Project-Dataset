
# Function for richness estimation

estimate_richness <- function(data_for_richness_estimation){
  assertthat::assert_that(
  is.data.frame(data_for_richness_estimation),
  msg = "data_for_richness_estimation has to be a data.frame"
  )
  
  data_for_richness_estimation %>% 
    mutate(present = ifelse(pollen_grains >= 1, 1, 0)) %>% 
    group_by(dataset_id,age) %>% 
    summarize(richness = sum(present, na.rm = TRUE, .groups = NULL))
}

