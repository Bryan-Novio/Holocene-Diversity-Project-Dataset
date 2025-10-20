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

binned_data_400_11_re <- read_rds(here("Outputs/Data/paper_1_study_2/binned_data_400_11_re.rds"))


pollen_data_s2 <-  read_rds(here("Outputs/Data/paper_1_study_2/datasub_p1_s2_counts_ages.rds"))
woody_taxa <- read_csv("Data/Processed/Other/woody_taxa_simova.csv", show_col_types = FALSE)
neotoma_taxa <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)
all_taxa_height_databases <- read_csv(here("Data/Processed/Other/all_taxa_height_databases.csv")) # from BIEN & TRY 


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

View(woody_taxa_re)

woody_taxa_re %>% unique()

# Extract all unique taxa from rawdataset

binned_data_400_11_re_taxa <- binned_data_400_11_re %>% distinct(taxa) 

study_2_taxa <- inner_join(binned_data_400_11_re_taxa, neotoma_taxa, by = join_by("taxa"=="taxon_name") ) %>% distinct(neotoma_names) 

woody_taxa_match <- inner_join(woody_taxa_re, study_2_taxa, by = join_by("taxa"=="neotoma_names")) # 133 woody match 

woody_taxa_non_match <- anti_join(woody_taxa_re,study_2_taxa, by = join_by("taxa"=="neotoma_names")) # 259 from Simova et al list but not in study_taxa list 

study_2_taxa_re <-  study_2_taxa %>%  rename(taxa = neotoma_names)

un_taxa <- anti_join(study_2_taxa_re, woody_taxa_re, by = "taxa") %>% distinct(taxa) # 243 unclassified taxa

un_taxa_re <- read_csv(here("Data/Processed/Other/unclass_taxa_re.csv")) # read now classified taxa based on web search # 242 only because of duplicate of (cf.) Oxyria digyna

un_taxa_re_woody <- un_taxa_re %>% filter(woodiness == "woody") %>%  select(taxa) # select on woody taxa from web search = 87 only

study_2_taxa_final <- bind_rows(woody_taxa_match,un_taxa_re_woody) %>% distinct() # combine woody taxa identified using Simova list with identified by web search


#----------------------------------------------------------#
# 5. Save reclassified harmonized taxa  ------------------
#----------------------------------------------------------# 

write_csv(un_taxa, here("Data/Processed/Other/unclass_taxa.csv"))
write_csv(study_2_taxa_final, here("Data/Processed/Other/study_2_taxa_final.csv"))

