#----------------------------------------------------------#     
#               
#               Holocene Diversity Project
#
#
#            Paper01| Method 2: Simova et al
#
#                       
#                          2023
# North America, site-based richness (dataset_id,age, 
# 1000 bins - rarefy 400 
#
#
#           ----  SELECT WOODY TAXA  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 


pollen_data_s2 <-  read_rds(here("Outputs/Data/paper_1_study_2/datasub_p1_s2_counts_ages.rds"))
woody_taxa <- read_csv("Data/Processed/Other/woody_taxa_simova.csv", show_col_types = FALSE)
neotoma_taxa <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)


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

source_files <- sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)


#----------------------------------------------------------#
# 2. Reclassify harmonized dataset using woody taxa list ---
#----------------------------------------------------------# 

woody_taxa_re <- woody_taxa %>% rename(taxa ='Taxon name orig.') %>% select(taxa)

woody_taxa_re %>% unique()

# Extract all unique taxa from harmonized dataset

pollen_data_s2_re <- pollen_data_s2 %>% rename(taxon_name =  taxa) 

study_2_taxa <- inner_join(pollen_data_s2_re,neotoma_taxa, by = "taxon_name" ) %>% distinct(neotoma_names)

woody_taxa_match <- inner_join(woody_taxa_re,study_2_taxa, by = join_by("taxa"=="neotoma_names"))

woody_taxa_non_match <- anti_join(woody_taxa_re,study_2_taxa, by = join_by("taxa"=="neotoma_names"))

woody_taxa_non_match_re <- read_csv(here("Data/Processed/Other/woody_taxa_non_match.csv")) %>% 
                           filter(woody_web_search =='yes') %>% 
                           select(taxa)
                



#----------------------------------------------------------#
# 5. Save reclassified harmonized taxa  ------------------
#----------------------------------------------------------# 

write_csv(woody_taxa_match, here("Data/Processed/Other/woody_taxa_res.csv"))
write_csv(woody_taxa_non_match, here("Data/Processed/Other/woody_taxa_non_match.csv"))

