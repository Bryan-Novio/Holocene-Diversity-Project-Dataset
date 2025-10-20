harmonize_taxa_s2_01 <- function(data_to_harmonize, neotoma_taxa, study_2_harmonized) {
  
  prep_data_study_ages_pollen <- data_to_harmonize %>%  join_data_prep_ages_pollen()
  
  data_to_harmonize %>%
    pull(taxa) %>%
    unique() %>%
    tibble::tibble() %>%
    as_vector() %>%
    as_tibble() %>%
    rename(taxon_name = value) %>%
    inner_join(neotoma_taxa, by = "taxon_name") %>%
    select(neotoma_names) %>%
    rename(taxon_name = neotoma_names) %>%
    inner_join(study_2_harmonized, by = join_by("taxon_name" == "pollen_type")) %>%
    select(taxon_name) %>%
    inner_join(prep_data_study_ages_pollen, by = "taxon_name", relationship = "many-to-many") %>%
    rename(taxa = taxon_name) %>% 
    group_by(dataset_id, sample_id, age,taxa) %>%
    summarize(pollen_sum = sum(pollen_counts), .groups = "drop") %>% 
    rename(pollen_counts = pollen_sum) %>% 
    drop_na()
}
