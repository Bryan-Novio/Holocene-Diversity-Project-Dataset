

rarefy_one_sample <- function(data_source_inner, sel_sample_id, n_grains_inner) {
  test_sample <- data_source_inner %>% 
    filter(sample_id == !!sel_sample_id) %>% 
    filter(!is.na(pollen_counts), pollen_counts > 0)
  
  if(nrow(test_sample) == 0) return(NULL)
  
  # replication without unnesting
  replicated_taxa <- rep(test_sample$taxa, test_sample$pollen_counts)
  
  if (length(replicated_taxa) < n_grains_inner) {
    warning(paste("Sample", sel_sample_id, "has only", length(replicated_taxa), "grains."))
    return(NULL)
  }
  
  sampled <- sample(replicated_taxa, n_grains_inner, replace = FALSE)
  
  test3 <- as.data.frame(table(sampled), stringsAsFactors = FALSE)
  colnames(test3) <- c("taxon_name", "n_pollen_grains")
  test3$dataset_id <- test_sample$dataset_id[1]
  test3$sample_id <- sel_sample_id
  
  return(test3)
}







