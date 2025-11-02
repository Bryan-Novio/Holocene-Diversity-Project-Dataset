#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                      ALL STUDIES
#
#       ---- CREATE CUSTOM HARMONIZATION TABLE ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(purrr)
library(dplyr)
library(taxospace)

#PART 1: CLASSIFY STUDY TAXA

#----------------------------------------------------------#
# 1. Load datasubset files per study------------------
#----------------------------------------------------------#

datasubset_all  <- list.files("Outputs/Data/p1_datasubset/", pattern ="[.]rds$", full.names = TRUE)

taxa_ref_table <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

#----------------------------------------------------------#
# 2. Select unique taxa from all dataset from each study ------
#----------------------------------------------------------#

datasubset_all_unique <- 
  purrr::map(datasubset_all,readr::read_rds) %>%
  bind_rows() %>% 
  distinct(taxa) %>% 
  rename(taxon_name = taxa)

#----------------------------------------------------------#
# 3. Convert taxon names to neotoma names
#----------------------------------------------------------#

datasubset_all_unique_re <- 
  inner_join(datasubset_all_unique,taxa_ref_table, by = "taxon_name") %>% 
  select(neotoma_names) %>%
  rename(taxon_name = neotoma_names) %>% 
  distinct()

datasubset_all_unique_re_vec <- 
  as.vector.data.frame(datasubset_all_unique_re)

datasubset_all_unique_re_vec_chr <- 
  unlist(datasubset_all_unique_re_vec) %>% 
  as.character() 

#----------------------------------------------------------#
# 4. Get classification per taxon list 
#----------------------------------------------------------#

safe_classification <- purrr::safely(get_classification) # capture errors 

class_taxa <- purrr::map(datasubset_all_unique_re_vec_chr, ~ safe_classification
  (taxa_vec = .x, use_only_exact_match = FALSE), 
  .progress = TRUE) 

class_taxa_tib <- tibble::tibble(class_taxa)  # classification as tibble

#----------------------------------------------------------#
# 5. Save  class_taxa as RDS file
#----------------------------------------------------------#

write_rds(class_taxa, here("Data/Processed/Data_harmonised/classified_taxa_neotoma_p1_all_subset.rds"))


#PART 2: CLASSIFY FAILED TAXA


#----------------------------------------------------------#
# 1. Load data classified taxa rds file  ------------------
#----------------------------------------------------------#

class_taxa <- read_rds(here("Data/Processed/Data_harmonised/classified_taxa_neotoma_p1_all_subset.rds"))

class_taxa_tib <- tibble::tibble(class_taxa)  # classification as tibble
class_taxa_tib %>% unnest(class_taxa)

#----------------------------------------------------------#
# 2. Identify taxa with successful or failed classifications
#----------------------------------------------------------#

class_taxa_tib_has_no_result <- function(x) {   # function to id taxon with failed classifications
  is.null(x$result)
}

class_taxa_checked <-                           # produce checklist of successful or failed classifications: TRUE -> failed
  class_taxa_tib %>% 
  dplyr::mutate(
    has_no_results = purrr::map_lgl(
      .x = class_taxa,.f =
        class_taxa_tib_has_no_result
    )
  ) 

class_taxa_checked %>%  print(n = 500)

class_taxa_has_no_res <- class_taxa_checked %>%
  select(has_no_results)  # select on classification results column as lgl

class_taxa_has_no_res_chr <- class_taxa_has_no_res %>% 
  as_vector() %>% 
  as.character() %>% 
  as_tibble() # as chr

class_taxa_names <- bind_cols(datasubset_all_unique_re,class_taxa_has_no_res_chr) %>%
  rename(class_res = value)  # bind neotoma_name with classification results 

class_taxa_failed <- class_taxa_names %>% 
  filter(class_res == TRUE)  # list of failed classifications

class_taxa_success <- class_taxa_names %>%
  filter(class_res == FALSE)   # list of succesful classifications

#----------------------------------------------------------#
# 3. Filter out if the classification is plant or an animal 
#----------------------------------------------------------#  

class_taxa_succeeded <- class_taxa_checked %>% # successful classification with tabulate results
  filter(has_no_results == FALSE) %>% 
  unnest(class_taxa) %>% 
  unnest_longer(class_taxa) %>% 
  unpack(cols = class_taxa) %>% 
  unnest_longer(class_taxa$classification) %>% 
  filter(!is.na(id))

class_taxa_animals <- class_taxa_succeeded %>%  # with classification Animalia 
  select(sel_name,classification) %>% 
  unnest_wider(classification) %>% 
  select(sel_name, name, rank) %>% 
  unnest(c(name,rank)) %>% 
  filter(rank == "kingdom") %>% 
  filter(name == "Animalia")

class_taxa_plants <- class_taxa_succeeded %>%  # with classification Plantae 
  select(sel_name,classification) %>% 
  unnest_wider(classification) %>% 
  select(sel_name, name, rank) %>% 
  unnest(c(name,rank)) %>% 
  filter(rank == "kingdom") %>% 
  filter(name == "Plantae")


write_rds(class_taxa_succeeded, here("Data/Processed/Data_harmonised/class_taxa_succeeded.rds"))
write_csv(class_taxa_plants, here("Data/Processed/Data_harmonised/class_taxa_plants.csv"))

#----------------------------------------------------------#
# 4. Manual classification ; 0 failed ; 108 classified as animals initially)
#----------------------------------------------------------#  

animals <- class_taxa_animals %>%
  rename(taxon_name = sel_name)

animals <- write_csv(animals,here("Data/Processed/Data_harmonised/animals.csv"))

animals_classified <- write_csv(animals_classified,here("Data/Processed/Data_harmonised/animals_classified.csv"))


animals_classified_names <- names((animals_classified))
get_animals_classified_names <- animals_classified  %>%  rename(level_= animals_classified_names, -1) 
new_name <- paste0("level_", 1:8)
animals_reclass <- get_animals_classified_names %>% set_names("taxon_name", new_name)

write_csv(animals_reclass, here("Data/Processed/Data_harmonised/additional_class_taxa.csv")) # classified as animals before then reclassified as animals

#STEP 3: CREATE CUSTOM HARMONISATION TABLE

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

