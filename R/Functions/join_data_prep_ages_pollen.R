
join_data_prep_ages_pollen <- function(data_prepared) {   # function to join prepared data with neotoma to rename taxa column & join with ages and pollen abundance
  data_prepared %>%
    rename(taxon_name = taxa) %>% 
    inner_join(neotoma_taxa, by = "taxon_name") %>% 
    select(dataset_id, age, pollen_counts, sample_id, neotoma_names) %>% 
    rename(taxon_name = neotoma_names)
}
