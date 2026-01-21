#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 3: Gordon et al
#
#                       
#                          2024
#
# North America & Europe, site-based richness (dataset_id,age, 
# 500 bins - rarefy 300 
#
#                 ----HARMONIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(assertthat)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

pollen_data_s3 <-  
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s3_counts_ages.rds"))

harmonisation_table <- 
  readr::read_csv(  
    here::here("Data/Paper_1/data_harmonize/harmonization_table_all_studies.csv")
  ) %>% 
  rename(taxon_name = neotoma_names)

neotoma_taxa <- 
  readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

### Birks harmonisation tables

birks_harm_tables <- 
  list.files("Data/Paper_1/data_supplementary", pattern = "06\\.csv$", full.names = TRUE) %>%
  purrr::map(read_csv)


Asia_levant <- birks_harm_tables[[1]] 
Asia_main <- birks_harm_tables[[2]]
Asia_siberia <- birks_harm_tables[[3]]

hlist_birks_asia <- bind_rows(Asia_levant, Asia_main, Asia_siberia) %>% 
  select(-taxon_name) %>% 
  rename(level = level_1)

hlist_birks_europe <- birks_harm_tables[[4]]  %>% 
  select(-taxon_name, - level_1) %>% 
  rename(level = level_2)

hlist_birks_namerica <- birks_harm_tables[[5]] %>% 
  select(-taxon_name) %>% 
  rename(level = level_1)

# Check mismatch from study hlist with birks hlist

not_in_harm_table_asia <- 
  anti_join(hlist_birks_asia,harmonisation_table, join_by (raw_name == taxon_name)) 

not_in_harm_table_europe <- 
  anti_join(hlist_birks_europe,harmonisation_table, join_by (raw_name == taxon_name)) 

not_in_harm_table_namerica <- 
  anti_join(hlist_birks_namerica,harmonisation_table, join_by (raw_name == taxon_name))

# Merge unique from birks to orig hlist

harmonisation_table_new_asia <- 
full_join(harmonisation_table, not_in_harm_table_asia, join_by(taxon_name == raw_name)) %>% 
  mutate(level_to_harm  = coalesce(level_6, level)) %>% 
  # remove taxa duplicates (n = 140)
  distinct(taxon_name, .keep_all = TRUE)


harmonisation_table_new_europe <- 
  full_join(harmonisation_table, not_in_harm_table_europe, join_by(taxon_name == raw_name)) %>% 
  mutate(level_to_harm  = coalesce(level_6, level)) # no duplicates detected

harmonisation_table_new_namerica <- 
  full_join(harmonisation_table, not_in_harm_table_namerica, join_by(taxon_name == raw_name)) %>% 
  mutate(level_to_harm  = coalesce(level_6, level)) # no duplicates detected

#----------------------------------------------------------#
# 2. Load functions ---------------------------------------
#----------------------------------------------------------#

# Get a vector of general functions

fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "\\.R$",
    recursive = TRUE
  )

# Load the function into the global environment

source_files <-
  sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

#----------------------------------------------------------#
# 3.Filter data by region and rename taxa to neotoma names 
#----------------------------------------------------------# 
#Rename taxa with neotoma name and filter by region

# Asia 
data_to_harmonize_asia <- 
  pollen_data_s3 %>% 
  filter(region =="Asia") %>% 
  inner_join(neotoma_taxa, by = 'taxon_name') %>% 
  select(dataset_id, sample_id,age, neotoma_names, pollen_counts) %>% 
  rename(taxon_name = neotoma_names)

#check all taxa in data present in harm table
harm_table_taxon_name_asia <- harmonisation_table_new_asia %>% distinct(taxon_name)
taxon_name_data_asia <- data_to_harmonize_asia %>% distinct(taxon_name)

anti_join(taxon_name_data_asia, harm_table_taxon_name_asia, by = 'taxon_name')

# Europe harmonization

data_to_harmonize_europe <- 
  pollen_data_s3 %>% 
  filter(region =="Europe") %>% 
  inner_join(neotoma_taxa, by = "taxon_name")%>% 
  select(dataset_id, sample_id,age, neotoma_names, pollen_counts) %>% 
  rename(taxon_name = neotoma_names)

#check all taxa in data present in harm table
harm_table_taxon_name_europe <- harmonisation_table_new_europe %>% distinct(taxon_name)
taxon_name_data_europe <- data_to_harmonize_europe %>% distinct(taxon_name)

anti_join(taxon_name_data_europe, harm_table_taxon_name_europe, by = 'taxon_name')

#NAmerica
data_to_harmonize_namerica <- 
  pollen_data_s3 %>% 
  filter(region == "North America") %>% 
  inner_join(neotoma_taxa, by = 'taxon_name') %>% 
  select(dataset_id, sample_id,age, neotoma_names, pollen_counts) %>% 
  rename(taxon_name = neotoma_names)

#check all taxa in data present in harm table
harm_table_taxon_name_namerica <- harmonisation_table_new_namerica %>% distinct(taxon_name)
taxon_name_data_namerica <- data_to_harmonize_namerica %>% distinct(taxon_name)

anti_join(taxon_name_data_namerica, harm_table_taxon_name_namerica, by = 'taxon_name')

# Harmonize taxa for each region

data_study3_harmonised_asia <-
  harmonize_taxa(
    data_to_harmonize = data_to_harmonize_asia,
    harmonisation_table = harmonisation_table_new_asia,
    level = "level_to_harm") %>% 
    rename(taxa = taxon_name) # no 'delete' in taxa

data_study3_harmonised_asia %>% filter(taxa == 'delete')

data_study3_harmonised_europe <-
  harmonize_taxa(
    data_to_harmonize = data_to_harmonize_europe,
    harmonisation_table = harmonisation_table_new_europe,
    level = "level_to_harm") %>% 
    rename(taxa = taxon_name) # do

data_study3_harmonised_namerica <-
  harmonize_taxa(
    data_to_harmonize = data_to_harmonize_namerica,
    harmonisation_table = harmonisation_table_new_namerica,
    level = "level_to_harm") %>% 
    rename(taxa = taxon_name) # do

#----------------------------------------------------------#
# 1. Write the harmonized data to RDS files ----------------
#----------------------------------------------------------# 

write_rds(data_study3_harmonised_asia, here("Data/Paper_1/data_harmonize/data_study3_harmonised_asia.rds"))

write_rds(data_study3_harmonised_europe, here("Data/Paper_1/data_harmonize/data_study3_harmonised_europe.rds"))

write_rds(data_study3_harmonised_namerica, here("Data/Paper_1/data_harmonize/data_study3_harmonised_namerica.rds"))
