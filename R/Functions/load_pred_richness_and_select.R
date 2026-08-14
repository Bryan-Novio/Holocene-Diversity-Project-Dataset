#@@ Function to load backtransformed data per iteration for study 3

load_pred_richness_and_select <-
  function( iteration, path = here("Data/Paper_1/data_model/data_back_2/"))
  {
    paste0(path, "/", iteration, ".rds") %>% 
      read_rds() %>% 
      select(region, age, richness)
  }