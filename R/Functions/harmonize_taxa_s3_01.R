harmonize_taxa_taxa_s3_01 <- function(data_to_harmonize, neotoma_taxa, harmonization_table) {

  
  prep_data_study_ages_pollen <- pollen_data_s3_eu %>%  join_data_prep_ages_pollen()
  
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
    inner_join(harmonization_table, by = "taxon_name") %>%
    select(taxon_name) %>%
    inner_join(prep_data_study_ages_pollen, by = "taxon_name", relationship = "many-to-many") %>%
    group_by(dataset_id, sample_id, age, taxon_name) %>%
    summarize(pollen_sum = sum(pollen_counts), .groups = "drop") %>% 
    rename(taxa = taxon_name) %>% 
    rename(pollen_counts = pollen_sum) %>% 
    drop_na()
}
