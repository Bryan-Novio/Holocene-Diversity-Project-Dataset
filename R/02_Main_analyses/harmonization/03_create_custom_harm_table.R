#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                      ALL STUDIES
#
#                     HARMONIZATION 
#             (Create harmonisation table)
#----------------------------------------------------------#
#
library(tidyverse)
library(here)
#
#----------------------------------------------------------#
# 1. Load data files ----
#----------------------------------------------------------#

data_classified_plants_successs <- 
  read_csv(here("Data/Processed/Paper_1/data_classified_plants_successs.csv"))

not_plants_classified <-
  read_csv(here("Data/Processed/Paper_1/not_plants_classified.csv"))

#----------------------------------------------------------#
# 2. Create general harmonization table ----
#----------------------------------------------------------#

classified_taxa_hlist_level <-
  data_classified_plants_successs %>% 
  select(neotoma_names, rank) %>% 
  distinct(rank)

classified_taxa_hlist_table <- 
  data_classified_plants_successs  %>% 
  select(neotoma_names, name, rank) %>% 
  pivot_wider(names_from = rank, values_from = name, names_sep = "_") %>% 
  mutate(across(where(is.list), 
  ~ sapply(., function(x) 
    if (length(x) == 0) NA_character_
          else paste(x, collapse = ", ")))) %>% 
  select(-c(subspecies,variety))


classified_taxa_hlist_table_filled_names <- 
  names((classified_taxa_hlist_table))

classified_taxa_hlist_table_filled_names <- 
  classified_taxa_hlist_table %>%  
  rename(level_= classified_taxa_hlist_table_filled_names, -1) 

new_name <- paste0("level_", 1:7)

classified_plants <- 
  classified_taxa_hlist_table_filled_names %>% 
  set_names("neotoma_names", new_name)

harmonization_table_all_studies <-
  bind_rows(classified_plants, not_plants_classified) 

#----------------------------------------------------------#
# 3.Save general harmonization table  ----
#----------------------------------------------------------#

write_csv(harmonization_table_all_studies, here("Data/Processed/Paper_1/harmonization_table_all_studies.csv"))
