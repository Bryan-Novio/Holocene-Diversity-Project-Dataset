#@@ Function to load backtransformed data per iteration for study 3

load_pred_richness_and_select <-
<<<<<<< HEAD
  function( iteration, path = here("Data/Paper_1/data_model/data_back_2/"))
  {
    paste0(path, "/", iteration, ".rds") %>% 
      read_rds() %>% 
      select(region, age, richness)
  }
=======
  function( iteration, path = here("Data/Paper_1/data_model/data_back_new/"))
    {
  paste0(path, "/", iteration, ".rds") %>% 
    read_rds() %>% 
    select(region, age, richness)
}
>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85
