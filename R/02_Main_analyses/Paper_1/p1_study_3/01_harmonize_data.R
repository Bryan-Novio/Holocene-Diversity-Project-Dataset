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

### Load harmonization table Birks harmonisation tables (doi: 10.6084/m9.figshare.24088194)

birks_harm_tables <- 
  list.files("Data/Paper_1/data_supplementary", pattern = "06\\.csv$", full.names = TRUE) %>%
  purrr::map(read_csv)

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
# 3. Create harm table for each region --
#----------------------------------------------------------# 

##3.1.convert first to neotoma_names

pollen_data_s3_neotoma <- 
  pollen_data_s3 %>% 
  distinct(taxa) %>% 
  left_join(.,neotoma_taxa, join_by("taxa" == "taxon_name")
  ) %>% 
  select(neotoma_names)

## 3.2.create harm table for taxa in data (Harm A)

pollen_data_taxa_harm_table <- 
  left_join(pollen_data_s3_neotoma, harmonisation_table, join_by("neotoma_names" == "taxon_name")) 


# Recreate Birks harm list per region

Asia_levant <- birks_harm_tables[[1]] 
Asia_main <- birks_harm_tables[[2]]
Asia_siberia <- birks_harm_tables[[3]]

##Asia
hlist_birks_asia <- 
  bind_rows(Asia_levant, Asia_main, Asia_siberia) %>% 
  select(-taxon_name) %>% 
  rename(level = level_1)

##Europe
hlist_birks_europe <- birks_harm_tables[[4]]  %>% 
  select(-taxon_name, - level_1) %>% 
  rename(level = level_2)

##North America
hlist_birks_namerica <- birks_harm_tables[[5]] %>% 
  select(-taxon_name) %>% 
  rename(level = level_1)

# 3.3.check for taxa in present in data but not in each birks harm table

##Asia
pollen_data_taxa_not_in_birks_asia <- 
  anti_join(pollen_data_s3_neotoma,hlist_birks_asia, join_by("neotoma_names" == "raw_name")) %>%  # 2,092 taxa missing
  select(neotoma_names)

##Europe

pollen_data_taxa_not_in_birks_europe <- 
  anti_join(pollen_data_s3_neotoma,hlist_birks_europe, join_by("neotoma_names" == "raw_name")) %>%  # 1,237 taxa missing
  select(neotoma_names)

##North America

pollen_data_taxa_not_in_birks_namerica <- 
  anti_join(pollen_data_s3_neotoma,hlist_birks_namerica, join_by("neotoma_names" == "raw_name")) %>%  # 1,872 taxa missing
  select(neotoma_names)

# 3.4. Create auxiliary harm table (Harm B)

##Asia 

birks_aux_harm_table_asia <- 
  left_join(pollen_data_taxa_not_in_birks_asia, harmonisation_table, join_by("neotoma_names" =="taxon_name")) %>% 
  select(neotoma_names, level_6) 

##Europe

birks_aux_harm_table_europe <- 
  inner_join(pollen_data_taxa_not_in_birks_europe, harmonisation_table, join_by("neotoma_names" =="taxon_name")) %>% 
  select(neotoma_names, level_6)

##Namerica

birks_aux_harm_table_namerica <- 
  inner_join(pollen_data_taxa_not_in_birks_namerica, harmonisation_table, join_by("neotoma_names" =="taxon_name")) %>% 
  select(neotoma_names, level_6)


##3.5.Merge auxiliary harm table with pollen_data_taxa_harm_table

##Asia
birks_aux_harm_table_asia_merged <-
  bind_rows(birks_aux_harm_table_asia, pollen_data_taxa_harm_table) %>%
  distinct(neotoma_names, .keep_all = TRUE) %>% #taxon_name is unique
  rename(taxon_name = neotoma_names) %>% 
  select(taxon_name, level_6)

##Europe

birks_aux_harm_table_europe_merged <-
  bind_rows(birks_aux_harm_table_europe, pollen_data_taxa_harm_table) %>%
  distinct(neotoma_names, .keep_all = TRUE) %>% #taxon_name is unique
  rename(taxon_name = neotoma_names) %>% 
  select(taxon_name, level_6)

##Namerica

birks_aux_harm_table_namerica_merged <-
  bind_rows(birks_aux_harm_table_namerica, pollen_data_taxa_harm_table) %>%
  distinct(neotoma_names, .keep_all = TRUE) %>% #taxon_name is unique
  rename(taxon_name = neotoma_names) %>% 
  select(taxon_name, level_6)

birks_aux_harm_table_europe_merged%>% distinct(level_6)
birks_aux_harm_table_namerica_merged %>% distinct(level_6)
birks_aux_harm_table_asia_merged %>% distinct(level_6)

#----------------------------------------------------------#
# 4. Harmonise taxa for each region --
#----------------------------------------------------------# 

## 4.1. Filter data by region and rename taxa to neotoma names 

#Rename taxa with neotoma name and filter by region

## Asia 
data_to_harmonize_asia <- 
  pollen_data_s3 %>% 
  filter(region =="Asia") %>% 
  inner_join(neotoma_taxa, join_by('taxa'== 'taxon_name')) %>% 
  select(dataset_id, sample_id,age, neotoma_names, pollen_counts) %>% 
  rename(taxon_name = neotoma_names)


##Europe

data_to_harmonize_europe <- 
  pollen_data_s3 %>% 
  filter(region =="Europe") %>% 
  inner_join(neotoma_taxa, join_by('taxa'== 'taxon_name'))%>% 
  select(dataset_id, sample_id,age, neotoma_names, pollen_counts) %>% 
  rename(taxon_name = neotoma_names)


##NAmerica
data_to_harmonize_namerica <- 
  pollen_data_s3 %>% 
  filter(region == "North America") %>% 
  inner_join(neotoma_taxa, join_by('taxa'== 'taxon_name')) %>%
  select(dataset_id, sample_id,age, neotoma_names, pollen_counts) %>% 
  rename(taxon_name = neotoma_names)

## 4.2. Harmonize taxa for each region

data_study3_harmonised_asia <-
  harmonize_taxa(
    data_to_harmonize = data_to_harmonize_asia,
    harmonisation_table = birks_aux_harm_table_asia_merged,
    level = "level_6") %>% 
  rename(taxa = taxon_name) # no 'delete' in taxa


data_study3_harmonised_europe <-
  harmonize_taxa(
    data_to_harmonize = data_to_harmonize_europe,
    harmonisation_table = birks_aux_harm_table_europe_merged ,
    level = "level_6") %>% 
    rename(taxa = taxon_name) # do

data_study3_harmonised_namerica <-
  harmonize_taxa(
    data_to_harmonize = data_to_harmonize_namerica,
    harmonisation_table = birks_aux_harm_table_namerica_merged,
    level = "level_6") %>% 
    rename(taxa = taxon_name) # no 'delete' in taxa

## 4.3. Merge all harmonised datasets

data_harmonised_merge <- 
  dplyr::bind_rows(
    data_study3_harmonised_asia %>% 
      dplyr::mutate(region = "Asia"),
    data_study3_harmonised_europe %>% 
      dplyr::mutate(region = "Europe"),
    data_study3_harmonised_namerica %>% 
      dplyr::mutate(region = "North_America"),
  )

#----------------------------------------------------------#
# 5. Write the harmonized data to RDS files ----------------
#----------------------------------------------------------# 

## 5.1. save new harm tables

write_csv(birks_aux_harm_table_asia_merged, here("Data/Paper_1/data_supplementary/study3_hlist_updated_Asia.csv"))

write_csv(birks_aux_harm_table_europe_merged, here("Data/Paper_1/data_supplementary/study3_hlist_updated_Europe.csv"))

write_csv(birks_aux_harm_table_namerica_merged, here("Data/Paper_1/data_supplementary/study3_hlist_updated_NAmerica.csv"))

## 5.2. save merged harmonised datasets

write_rds(data_harmonised_merge, here("Data/Paper_1/data_harmonize/data_study3_data_harmonised_merge.rds"))
