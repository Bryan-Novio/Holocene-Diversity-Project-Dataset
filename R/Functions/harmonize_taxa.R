harmonize_taxa <- function(data_to_harmonize, harmonisation_table, level) {
  assertthat::assert_that(
    is.data.frame(data_to_harmonize),
    msg = "data_to_harmonize has to be data.frame"
  )

  assertthat::assert_that(
    all(c("dataset_id", "sample_id", "age", "taxon_name", "pollen_counts") %in% colnames(data_to_harmonize)),
    msg = "data must contain columns 'dataset_id', 'sample_id', 'age', 'taxon_name', 'pollen_counts' "
  )

  taxa_level <- sym(level) # Convert string to symbol for use in dplyr

  harmonisation_table_prep <- harmonisation_table %>% 
    dplyr::select(taxon_name, !!taxa_level) %>%
    rlang::set_names(
      nm = c("taxon_name", "taxa_harmonised")
    )

  res <-
    data_to_harmonize %>%
    dplyr::inner_join(
      harmonisation_table_prep,
      by = "taxon_name"
    ) %>%
    dplyr::group_by(dataset_id, sample_id, age, taxa_harmonised) %>%
    dplyr::summarize(
      pollen_sum = sum(pollen_counts),
      .groups = "drop"
    ) %>%
    dplyr::rename(taxa = taxa_harmonised) %>%
    dplyr::rename(pollen_counts = pollen_sum)

  return(res)
}
