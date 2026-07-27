harmonize_taxa <- function(data_to_harmonize, harmonisation_table, level) {
  assertthat::assert_that(
    is.data.frame(data_to_harmonize),
    msg = "data_to_harmonize has to be data.frame"
  )
  
  assertthat::assert_that(
    all(c("dataset_id", "age", "taxon_name", "pollen_counts") %in% colnames(data_to_harmonize)),
    msg = "data must contain columns 'dataset_id', 'age', 'taxon_name', 'pollen_counts' "
  )
  
  taxa_level <- sym(level) # Convert string to symbol for use in dplyr
  
  harmonisation_table_prep <- harmonisation_table %>% 
    dplyr::select(taxon_name, !!taxa_level) %>%
    rlang::set_names(
      nm = c("taxon_name", "taxa_harmonised")
    ) %>% 
    dplyr::distinct()
  
  assert_that(
    duplicated(harmonisation_table_prep$taxon_name) %>% 
      any() == FALSE,
    msg = "there is duplication of taxon_name in harm table"
  )
  
  taxa_input <- 
    data_to_harmonize  %>% 
    dplyr::distinct(taxon_name) %>% 
    dplyr::mutate(
      present_in_data = TRUE
    )
  
  taxa_harmonisation_table <- 
    harmonisation_table_prep %>% 
    dplyr::distinct(taxon_name) %>% 
    dplyr::mutate(
      present_in_harm_table = TRUE
    )
  
  data_taxa_missing <- 
    full_join(
      taxa_input,
      taxa_harmonisation_table,
      by = "taxon_name"
    ) %>% 
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.logical),
        ~ replace_na(.x, FALSE)
      )
    ) %>% 
    dplyr::filter(
      present_in_data == TRUE &
        present_in_harm_table == FALSE
    )
  
  assertthat::assert_that(
    nrow(data_taxa_missing) == 0,
    msg = "harmonisation table must contain taxon_names"
  )
  
  res <-
    data_to_harmonize %>%
    dplyr::left_join(
      harmonisation_table_prep,
      by = "taxon_name"
    ) %>%
    dplyr::group_by(sample_id,dataset_id, age, taxa_harmonised) %>%
    dplyr::summarize(
      pollen_sum = sum(pollen_counts),
      .groups = "drop"
    ) %>%
    dplyr::rename(taxon_name = taxa_harmonised) %>%
    dplyr::rename(pollen_counts = pollen_sum)
  
  return(res)
}