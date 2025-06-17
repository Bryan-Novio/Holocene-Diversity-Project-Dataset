
# Function for richness estimation

estimate_richness <- function(data_for_richness_estimation){
  data_for_richness_estimation %>% 
    mutate(present = ifelse(avg_n_pollen_grains >= 1, 1, 0)) %>% 
    group_by(dataset_id) %>% 
    summarize(richness = sum(present, na.rm = TRUE, .groups = NULL))
}

