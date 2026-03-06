#' @title Rarefy dataset for iterations and save to drive
#' @description
#'We need to write to drive because otherwise we run out of memory
#'  
rarefy_all_samples_iter <- 
  
  function(n_iter, data_to_rarefy, path){

  1:n_iter %>% 
  purrr::set_names() %>% 
  purrr::walk(
    .progress = TRUE,
    .f = ~ rarefy_all_samples(data_to_rarefy, n_grains = 300) %>% 
    readr::write_rds(file = paste0(path, "/", .x, ".rds"))
    )
    
    # Explicitly clear local large objects and run gc()
    rm(data)
    gc(verbose = FALSE) 
    
}
    


