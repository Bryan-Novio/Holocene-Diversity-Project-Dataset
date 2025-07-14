library(tidyverse)
library(here)
library(purrr)
library(dplyr)

#----------------------------------------------------------#
# 1. Load up taxon list per study, classified taxon files
#----------------------------------------------------------# 

method_data_files    <- list.files("Data/Processed/Other/", pattern ="[.]rds$", full.names = TRUE)
class_taxa_success   <- read_rds(here("Data/Processed/Data_harmonised/class_taxa_succeeded.rds"))
class_taxa_plants    <- read_csv(here("Data/Processed/Data_harmonised/class_taxa_plants.csv"), show_col_types = FALSE)
taxa_ref_table       <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

#----------------------------------------------------------#
# 2. Extract taxon list from each study
#----------------------------------------------------------#

method_data_files

method_data <- purrr::map(method_data_files,readr::read_rds) 
method_data_taxa <- purrr::map(method_data, ~ pull(.x, taxa))  # extract taxon per study
str(method_data_taxa)

method_data_taxa_unique <- purrr::map(method_data_taxa, ~ unique(.x)) # select only unique taxa per taxon list 
str(method_data_taxa_unique[[1]])
str(method_data_taxa_unique[[2]])
str(method_data_taxa_unique[[3]])
str(method_data_taxa_unique[[4]])

method_data_taxa_unique_tib <- tibble::tibble(method_data_taxa_unique) 
str(method_data_taxa_unique_tib)

method_data_taxa_unique_tib_1 <- method_data_taxa_unique_tib[[1]][[1]] %>% as_vector() %>% as_tibble() %>% rename(taxon_name = value) # extract taxon list per study as tibble 
method_data_taxa_unique_tib_2 <- method_data_taxa_unique_tib[[1]][[2]] %>% as_vector() %>% as_tibble() %>% rename(taxon_name = value)
method_data_taxa_unique_tib_3 <- method_data_taxa_unique_tib[[1]][[3]] %>% as_vector() %>% as_tibble() %>% rename(taxon_name = value)
method_data_taxa_unique_tib_4 <- method_data_taxa_unique_tib[[1]][[4]] %>% as_vector() %>% as_tibble() %>% rename(taxon_name = value)


method_data_taxa_unique_join_neotoma_1 <- inner_join(method_data_taxa_unique_tib_1, taxa_ref_table, by ='taxon_name') %>% select(neotoma_names) %>% rename(taxon_name = neotoma_names) # translate each taxa to neotoma name
method_data_taxa_unique_join_neotoma_2 <- inner_join(method_data_taxa_unique_tib_2, taxa_ref_table, by ='taxon_name') %>% select(neotoma_names) %>% rename(taxon_name = neotoma_names)
method_data_taxa_unique_join_neotoma_3 <- inner_join(method_data_taxa_unique_tib_3, taxa_ref_table, by ='taxon_name') %>% select(neotoma_names) %>% rename(taxon_name = neotoma_names)
method_data_taxa_unique_join_neotoma_4 <- inner_join(method_data_taxa_unique_tib_4, taxa_ref_table, by ='taxon_name') %>% select(neotoma_names) %>% rename(taxon_name = neotoma_names)


#----------------------------------------------------------#
# 3. Create general harmonization table 
#----------------------------------------------------------#

class_taxa_hlist <- class_taxa_success %>% 
  select(sel_name,classification) %>% 
  inner_join(class_taxa_plants, by = "sel_name") %>%            
  select(sel_name, classification) %>% 
  unnest_wider(classification) %>% 
  select(sel_name, name, rank)

class_taxa_hlist_level <- class_taxa_hlist %>% 
  select(sel_name, rank) %>% 
  unnest(rank) %>% 
  distinct(rank)

class_taxa_hlist_init <- class_taxa_hlist %>% 
  select(sel_name, name) %>% 
  unnest_wider(name, names_sep = "_" ) 

class_taxa_hlist_final <-  class_taxa_hlist_init %>% 
  rename_with(.fn = ~ str_replace(.x,"name_", "level_"), 
              .cols = starts_with("name_")
  ) 

harmonization_table_gen <- class_taxa_hlist_final %>% rename(taxon_name = sel_name)

write_csv(harmonization_table_gen, here("Data/Processed/Data_harmonised/harmonization_table_gen.csv"))

#----------------------------------------------------------#
# 4. Create harmonization table per study
#----------------------------------------------------------# 

harmonization_table_s1 <- inner_join(method_data_taxa_unique_join_neotoma_1, harmonization_table_gen, by = "taxon_name")
harmonization_table_s2 <- inner_join(method_data_taxa_unique_join_neotoma_2, harmonization_table_gen, by = "taxon_name")
harmonization_table_s3 <- inner_join(method_data_taxa_unique_join_neotoma_3, harmonization_table_gen, by = "taxon_name")
harmonization_table_s4 <- inner_join(method_data_taxa_unique_join_neotoma_4, harmonization_table_gen, by = "taxon_name")

