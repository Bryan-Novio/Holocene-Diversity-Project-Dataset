#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                      ALL STUDIES
#
#                     HARMONIZATION 
#             (Create harmonisation table)
#
#----------------------------------------------------------#
# 1. Load data files ----
#----------------------------------------------------------#

classified_taxa_succeeded <- 
  read_rds(here("Data/Processed/Data_harmonised/classified_taxa_succeeded.rds"))

classified_taxa_plants <- 
  read_rds(here("Data/Processed/Data_harmonised/classified_taxa_plants.rds"))

not_plants_classified <- 
  read_csv(here("Data/Processed/Data_harmonised/not_plants_classified.csv"))

classified_taxa_hlist_table_filled <- 
  read_csv(here("Data/Processed/Data_harmonised/classified_taxa_hlist_table.csv"))

not_plants_classified_manually <-
  read_csv(here("Data/Processed/Data_harmonised/not_plants_classified_manually.csv"))


#----------------------------------------------------------#
# 2. Create general harmonization table ----
#----------------------------------------------------------#

classified_taxa_hlist <- 
  classified_taxa_succeeded %>% 
  select(sel_name,classification) %>% 
  inner_join(classified_taxa_plants, by =  "sel_name") %>%            
  select(sel_name, classification) %>% 
  unnest_wider(classification) %>% 
  select(sel_name, name, rank)

classified_taxa_hlist <- 
  classified_taxa_succeeded %>% 
  left_join(classified_taxa_plants, by = "neotoma_names") %>% 
  select(neotoma_names, name, rank)

class_taxa_hlist_level <-
  classified_taxa_hlist %>% 
  select(neotoma_names, rank) %>% 
  distinct(rank)

classified_taxa_hlist_table <- classified_taxa_hlist  %>% 
  select(neotoma_names, name, rank) %>% 
  pivot_wider(names_from = rank, values_from = name, names_sep = "_") %>% 
  mutate(across(where(is.list), 
  ~sapply(., function(x) if (length(x) == 0) NA_character_
          else paste(x, collapse = ", ")))) %>% 
  select(-c(subspecies,variety))


classified_taxa_hlist_table_filled_names <- 
  names((classified_taxa_hlist_table_filled))

classified_taxa_hlist_table_filled_names <- 
  classified_taxa_hlist_table_filled %>%  
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

write_rds(harmonization_table_all_studies, here("Data/Processed/Data_harmonised/harmonization_table_all_studies.rds"))

