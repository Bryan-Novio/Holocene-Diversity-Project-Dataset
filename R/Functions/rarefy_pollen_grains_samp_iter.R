

rarefy_pollen_grains_samp_iter <- function(data_source, sample_id, n_grains, n_iter) {
  
  # Get pollen data and filter to one sample
  pollen_sample  <- data_source %>%
    filter(sample_id == !!sample_id, pollen_grains > 0)
  
  # Expand pollen counts into repeated taxon names
  pollen_sample2 <- pollen_sample %>%
    filter(pollen_grains > 0) %>%
    mutate(replicated_taxa = map2(taxa, pollen_grains, rep)) %>%
    select(replicated_taxa) %>%
    unnest(replicated_taxa)
  
  if (nrow(pollen_sample2) < n_grains) {
    warning(paste("Sample", sample_id, "has fewer than", n_grains, "grains. Skipping."))
    return(NULL)
  }
  
  # Perform multiple rarefaction iterations using map() + list_rbind()
  iteration_list <- map(
    .x = 1:n_iter,
    .f = ~ {
      sampled_pollen <- sample(pollen_sample2$replicated_taxa, n_grains, replace = FALSE)
      as.data.frame(table(sampled_pollen)) %>%
        rename(taxon_name = sampled_pollen, n_pollen_grains = Freq) %>%
        mutate(iteration = .x)
    })
  
  iteration_results <- dplyr::bind_rows(iteration_list)
  
  # Calculate average pollen count per taxon across iterations
  summary <- iteration_results %>%
    group_by(taxon_name) %>%
    summarise(avg_n_pollen_grains = mean(n_pollen_grains), .groups = "drop") %>%
    mutate(dataset_id = unique(pollen_sample$dataset_id),
           sample_id = sample_id)
  
  return(summary)
}