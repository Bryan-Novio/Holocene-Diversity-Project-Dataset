#' @title Rarefy dataset for iterations and save to drive
#' @description
#'We need to write to drive because otherwise we run out of memory
#'  
rarefy_all_samples_iter <- function(n_iter, data_to_rarefy, path) {
  
  for (i in seq_len(n_iter)) {
    
    rarefy_all_samples(data_to_rarefy, n_grains = 300) %>%
      readr::write_rds(file = paste0(path, "/", i, ".rds"))
    
    # Optional: show progress
    message("Completed iteration ", i, " of ", n_iter)
    
    # Explicitly run gc()
    gc(verbose = FALSE)
  }
  
  

}