
harmonize_taxa <- function(data_to_harmonize, data_ancillary, level) {
  
  taxa_level <- sym(level)  # Convert string to symbol for use in dplyr
  
  data_ancillary <- list(neotoma_taxa = neotoma_taxa,
                         harmonization_table = harmonization_table)
  
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
    inner_join(harmonization_table, by = "taxon_name") %>%
    select(taxon_name, level_5, level_6, level_7, level_8) %>%
    inner_join(prep_data_study_ages_pollen, by = "taxon_name", relationship = "many-to-many") %>%
    group_by(dataset_id, sample_id, age, !!taxa_level) %>%
    summarize(pollen_sum = sum(pollen_grains), .groups = "drop") %>% 
    rename(taxa = starts_with("level_")) %>% 
    rename(pollen_counts = pollen_sum) %>% 
    drop_na()
}
