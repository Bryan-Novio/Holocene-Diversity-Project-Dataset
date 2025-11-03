
#PART 3: CREATE CUSTOM HARMONISATION TABLE----

additional_class_taxa <- read_csv(here("Data/Processed/Data_harmonised/additional_class_taxa.csv"))
class_taxa_success   <- read_rds(here("Data/Processed/Data_harmonised/class_taxa_succeeded.rds"))
class_taxa_plants    <- read_csv(here("Data/Processed/Data_harmonised/class_taxa_plants.csv"), show_col_types = FALSE)
taxa_ref_table       <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

#----------------------------------------------------------#
# 1. Create general harmonization table 
#----------------------------------------------------------#

class_taxa_hlist <- class_taxa_success %>% 
  select(sel_name,classification) %>% 
  inner_join(class_taxa_plants, by =  "sel_name") %>%            
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

harmonization_table_gen_combined <- bind_rows(harmonization_table_gen, additional_class_taxa) #

write_csv(harmonization_table_gen_combined, here("Data/Processed/Data_harmonised/harmonization_table_new.csv"))

