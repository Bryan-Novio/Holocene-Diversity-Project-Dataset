
bin_rarefy_estimate_richness_harmonized_data <- function(harmonized_data_level){
  
harmonized_data_level %>% 
    bin_data(500) %>%                      # binning 
    prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
    mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
    rarefy_all_samples_iter(                            # rarefaction
      data_source =.,
      n_grains = 500,
      n_iter = 10) %>% 
    separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
    estimate_richness() %>%                             # estimate richness
    mutate(age = as.numeric(age)) %>% 
    ggplot(aes(y = richness, x = age)) + 
    geom_point() +
    geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
    theme_classic()
}
  