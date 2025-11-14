#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                      ALL STUDIES
#
#                     HARMONIZATION 
#                    (Classify Taxa)
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(taxospace)

#----------------------------------------------------------#
# 1. Load datasubset files per study-----
#----------------------------------------------------------#

#Run first '00_subset_data script' to generate each study dataset

datasubset_all  <- 
  list.files(here("Outputs/Data/p1_datasubset/"), pattern ="[.]rds$", full.names = TRUE) %>% 
  purrr::map(readr::read_rds)

#Since Fossilpol taxon names needs to translated to Neotoma names, we need to load the taxa reference table

taxa_ref_table <-
  readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

#----------------------------------------------------------#
# 2. Select unique taxa from all dataset from each study ------
#----------------------------------------------------------#

datasubset_all_unique <-
  datasubset_all %>%
  bind_rows() %>% 
  distinct(taxa) %>% 
  rename(fossilpol_name = taxa)

#----------------------------------------------------------#
# 3. Convert taxon names to neotoma names ----
#----------------------------------------------------------#

datasubset_all_unique_neotoma_names <- 
  inner_join(datasubset_all_unique,taxa_ref_table, by = join_by("fossilpol_name" =="taxon_name")) %>% 
  select(neotoma_names) %>%
  distinct()

datasubset_all_unique_neotoma_names_vec <- 
  unlist(datasubset_all_unique_neotoma_names) %>% 
  as.character() 

#----------------------------------------------------------#
# 4. Get classification per taxon -----
#----------------------------------------------------------#

safe_classification <- purrr::safely(taxospace::get_classification)  # capture errors 

classified_taxa <- 
  datasubset_all_unique_neotoma_names %>% 
  mutate(
    classification = purrr::map(
      neotoma_names,
      ~ safe_classification(
          taxa_vec = .x,
          use_only_exact_match = FALSE), 
      .progress = TRUE) )

#----------------------------------------------------------#
# 5. Save classified_taxa as RDS file -----
#----------------------------------------------------------#

write_rds(classified_taxa, here("Data/Paper_1/classified_taxa_neotoma_p1_all_subset.rds"))
