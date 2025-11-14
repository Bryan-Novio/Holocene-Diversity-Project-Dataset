#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                      ALL STUDIES
#
#                     HARMONIZATION 
#                (Classify failed taxa)
#
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data classified taxa rds file  ----
#----------------------------------------------------------#

classified_taxa <-
  read_rds(here("Data/Processed/Paper_1/classified_taxa_neotoma_p1_all_subset.rds"))

#----------------------------------------------------------#
# 2. Load functions ----
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
# 3. ID taxa with successful or failed classifications----
#----------------------------------------------------------#

# produce checklist of successful or failed classifications: TRUE -> failed

classified_taxa_checked <-    
  classified_taxa %>% 
  dplyr::mutate(
    has_no_results = purrr::map_lgl(
      .x = classification ,.f =
        return_taxa_not_classified
    )
  ) 

# select on classification results column as logical

classified_taxa_has_no_res <- 
  classified_taxa_checked %>%
  filter(has_no_results==TRUE) %>% 
  select(neotoma_names) %>% 
  distinct()

# list of successful classifications

classified_taxa_success <- 
  classified_taxa_checked %>%
  filter(has_no_results==FALSE) %>% 
  select(neotoma_names) %>% 
  distinct()

#----------------------------------------------------------#
# 4. Filter classification as plant or not plants ----
#----------------------------------------------------------#  

# successful classification with tabulated results

classified_taxa_succeeded <-
  classified_taxa_checked %>% 
  filter(has_no_results == FALSE) %>% 
  select(neotoma_names, classification) %>% 
  unnest(classification) %>% 
  unnest(classification) %>% 
  select(neotoma_names, classification) %>% 
  unnest(classification) %>% 
  filter(!is.na(id))

# with classification Plantae

classified_taxa_plants <- 
  classified_taxa_succeeded %>%  
  filter(rank == "kingdom") %>% 
  filter(name == "Plantae") %>% 
  mutate(is_plant = TRUE) %>% 
  select(neotoma_names, is_plant)


data_classified_plants_successs <- 
  classified_taxa_succeeded %>% 
  left_join(classified_taxa_plants, by = "neotoma_names") %>% 
  select(neotoma_names, name, rank)

#----------------------------------------------------------#
# 5. Detect of failed taxa ----
#----------------------------------------------------------#  

# 758 failed to be classified and 15 classified as not_plants initially = 773 taxa need manual classification

not_plants <- 
  classified_taxa_succeeded %>% 
  distinct(neotoma_names) %>% 
  left_join(
    classified_taxa_plants
    ) %>% 
  dplyr::mutate(
    is_plant = case_when(
      .default = is_plant,
      is.na(is_plant) ~ FALSE
    )
  ) %>% 
  filter(is_plant ==FALSE)

# combine failed classification and classified as not_plants

not_plants_to_classify_manual <- 
  bind_rows(classified_taxa_has_no_res, not_plants) %>% 
  select(neotoma_names)


write_csv(not_plants_to_classify_manual, here("Data/Processed/Paper_1/taxa_to_classify.csv"))

# load failed taxa manually classified

not_plants_classified_manually <-
  read_csv(here("Data/Processed/Paper_1/taxa_to_classify_filled.csv"))

# get col names from not_plants_classified_manually

not_plants_classified_manually_names <- 
  names(all_of(not_plants_classified_manually))

# rename ranks as level_ (level_6 = "genus")

rename_not_plants_classified_names <- 
  not_plants_classified_manually %>%  
  rename(level_= not_plants_classified_manually_names, -1) 

new_name <- paste0("level_", 1:7)

not_plants_classified <- 
  rename_not_plants_classified_names %>% 
  set_names("neotoma_names", new_name)

#----------------------------------------------------------#
# 6. Save classified taxa files ----
#----------------------------------------------------------#  

write_csv(data_classified_plants_successs, here("Data/Processed/Paper_1/data_classified_plants_successs.csv"))
write_csv(not_plants_classified, here("Data/Processed/Paper_1/not_plants_classified.csv"))

