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

harmonized_data_study_2 <- read_rds(here("Outputs/Data/paper_1_study_2/harmonized_data_study_2.rds"))
woody_taxa <- read_csv("Data/Processed/Other/woody_taxa_simova.csv", show_col_types = FALSE)

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

woody_taxa_re <- woody_taxa %>% rename(taxa ='Pollen type') %>% select(taxa)

View(woody_taxa_re)


raw_taxa <- purrr::map(list(harmonized_data_study_2$genus, harmonized_data_study_2$species), ~dplyr::select(.x, taxa)) %>% 
                       unlist() %>%  as_tibble_col() %>% unique() %>% 
                       rename(taxa=value)

woody_taxa <- inner_join(woody_taxa_re,raw_taxa, by = "taxa")

#----------------------------------------------------------#
# 5. Save reclassified harmonized taxa  ------------------
#----------------------------------------------------------# 

write_csv(woody_taxa, here("Data/Processed/Other/woody_taxa_res.csv"))


