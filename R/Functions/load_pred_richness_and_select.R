#@@ Function to load backtransformed data per iteration for study 3


  function( iteration, path = here("Data/Paper_1/data_model/data_back_new/"))
    {
  paste0(path, "/", iteration, ".rds") %>% 
    read_rds() %>% 
    select(region, age, richness)
}
