
rarefy_pollen_grains_sample <- function(data_source, sample_id, n_grains){
  pollen_sample<-
    get_pollen_counts_with_ages(data_source)  # Load all pollen data (with ages and counts)
  # Filter the full dataset to keep only the specified sample
  pollen_sample1 <-                                   
    pollen_sample %>% 
    filter(sample_id == !!sample_id)        
  # Prepare for rarefaction:
  pollen_sample2<-                                  
    pollen_sample1 %>% 
    filter(pollen_counts> 0) %>%   # Keep only taxa with >0 pollen grains           
    mutate(
      replicated_taxa = purrr::map2(
        .x = taxa,
        .y = pollen_counts,
        .f = rep
      )
    )
  # Unnest the list of replicated taxa into long format
  pollen_sample3 <-                                   
    pollen_sample2 %>% 
    select(dataset_id, sample_id, replicated_taxa) %>% 
    unnest(replicated_taxa)
  # Perform rarefaction by sampling grains
  pollen_sample4 <-  sample(pollen_sample3$replicated_taxa, n_grains, replace = FALSE) %>%  
    table() %>%                                         
    as.data.frame()                                    
  
  colnames(pollen_sample4) <- c("taxon_name", "n_pollen_grains")
  
  return(pollen_sample4)
  #  Return the rarefied sample                           
}

