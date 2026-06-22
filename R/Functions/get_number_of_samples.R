
#function to get mean number of samples and sd

get_number_of_samples <- function(study_dataset){
  study_dataset %>% 
    group_by(dataset_id) %>% 
    distinct(sample_id) %>% 
    summarize(n = n()) %>% 
    summarize(mean_sample =  mean (n),
              sd = sd(n))
}
  