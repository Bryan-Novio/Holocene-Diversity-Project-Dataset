get_pollen_ages <- function(data_compilation){      # function to obtain age for each dataset id
  assertthat::assert_that(
    is.data.frame(data_compilation),
    msg = "data_compilation has to be a data.frame"
  )
  
  data_compilation %>% 
    select(dataset_id, levels) %>% 
    unnest(levels) %>% 
    select(dataset_id,sample_id, age)
}