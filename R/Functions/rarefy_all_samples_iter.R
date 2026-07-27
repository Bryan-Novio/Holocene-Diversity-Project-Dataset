rarefy_all_samples_iter <- function(data_to_rarefy, n_iter = 1000, path) {
  
  1:n_iter %>% 
    purrr::set_names() %>% 
    purrr::walk(
      .progress = TRUE,
      .f = function(x) {
        file_path <- file.path(path, paste0(x, ".rds"))
        
        # SKIP LOGIC: If file exists and is not empty, skip to next iteration
        if (file.exists(file_path) && file.info(file_path)$size > 0) {
          return(NULL)
        }
        
        # Otherwise, run rarefaction and save
        rarefy_all_samples(data_to_rarefy, n_grains = 300) %>% 
          readr::write_rds(file = file_path)
      }
    )
}