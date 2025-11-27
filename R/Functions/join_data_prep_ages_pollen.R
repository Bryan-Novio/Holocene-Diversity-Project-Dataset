
join_data_prep_ages_pollen <- function(data_prepared) {
  assertthat::assert_that(
    is.data.frame(data_prepared),
    msg = "data_prepared has to be a data.frame"
  )
  
  data_prepared %>%
    rename(taxon_name = taxa) %>% 
    inner_join(neotoma_taxa, by = "taxon_name") %>% 
    select(dataset_id, age, pollen_counts, sample_id, neotoma_names) %>% 
    rename(taxon_name = neotoma_names)
}
