

rarefy_pollen_grains_all_samp_iter <- function(data_source, dataset_id, n_grains, n_iter) {
  # Load and filter data for the dataset
  all_pollen <- get_pollen_counts_with_ages(data_source) %>%
    filter(dataset_id == !!dataset_id, pollen_counts > 0)
  
  # Get unique sample IDs in the dataset
  sample_ids <- unique(all_pollen$sample_id)
  
  #  rarefy one sample
  rarefy_one_sample <- function(sample_id) {
    sample_data <- all_pollen %>% filter(sample_id == !!sample_id)
    
    # Expand pollen counts into a  vector
    replicated_taxa <- rep(sample_data$taxa, sample_data$pollen_counts)
    
    # Check if sufficient grains are available
    if (length(replicated_taxa) < n_grains) {
      warning(paste("Sample", sample_id, "has only", length(replicated_taxa), "grains. Skipping."))
      return(NULL)
    }
    
    # Perform multiple iterations of rarefaction
    pollen_iter_list <- map(1:n_iter, function(i) {
      sampled <- sample(replicated_taxa, n_grains, replace = FALSE)
      as.data.frame(table(sampled), stringsAsFactors = FALSE) %>%
        rename(taxon_name = sampled, n_pollen_grains = Freq) %>%
        mutate(iteration = i)
    })
    
    # Combine iterations and summarize
    pollen_iter_res <- bind_rows(pollen_iter_list)
    
    summary <- pollen_iter_res %>%
      group_by(taxon_name) %>%
      summarise(avg_n_pollen_grains = mean(n_pollen_grains), .groups = "drop") %>%
      mutate(dataset_id = dataset_id, sample_id = sample_id)
    
    return(summary)
  }
  
  # Apply rarefaction to all samples in the dataset
  res_list <- map(sample_ids, rarefy_one_sample)
  final_res <- bind_rows(res_list)
  
  return(final_res)
}
