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

data_study2_binned <-
  read_rds(
    here("Data/Paper_1/data_bin/data_study2_binned.rds")
  )

# This is as supplementary file from DOI: 10.1111/geb.13649 

woody_taxa <- 
  read_csv("Data/Paper_1/data_supplementary/woody_taxa_simova.csv", show_col_types = FALSE)


neotoma_taxa <- 
  readr::read_csv(
  here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"),
  show_col_types = FALSE
)

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

source_files <-
  sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)


#----------------------------------------------------------#
# 2. Reclassify harmonized dataset using woody taxa list ---
#----------------------------------------------------------#

woody_taxa_renamed <-
  woody_taxa %>%
  rename(taxa = "Taxon name orig.") %>%
  select(taxa)

# Extract all unique taxa from rawdataset

data_study2_binned_taxa <- 
  data_study2_binned  %>%
  distinct(taxa)

study_2_taxa <- inner_join(
  data_study2_binned_taxa,
  neotoma_taxa,
  by = join_by("taxa" == "taxon_name")
) %>%
  distinct(neotoma_names)

woody_taxa_match <- 
  inner_join(
  woody_taxa_renamed,
  study_2_taxa,
  by = join_by("taxa" == "neotoma_names")
)

# -> It now has 123 woody match


unclassified_if_woody_taxa <- 
  anti_join(
  study_2_taxa,
  woody_taxa,
  by = join_by("neotoma_names" == 'Taxon name orig.')) %>% 
  rename(taxa = neotoma_names) %>% 
  distinct(taxa) # 227 unclassified taxa

# save the unmatched taxa

write_csv(unclassified_if_woody_taxa, here("Data/Paper_1/data_supplementary/unclassified_if_woody_taxa_study2.csv"))

#----------------------------------------------------------#
# 3. Manual search for the unmatched taxa  ------------------
#----------------------------------------------------------#

# We have individually assing the unmatched taxa and
#   saved as `unclass_taxa_filled.csv`

#----------------------------------------------------------#
# 4. Manual search for the unmatched taxa  ------------------
#----------------------------------------------------------#

# read now classified taxa based on web search # 242 only because of duplicate of (cf.) Oxyria digyna

unclassified_if_woody_taxa_study2_filled <- 
  read_csv(here("Data/Paper_1/data_supplementary/unclassified_if_woody_taxa_study2_filled.csv"))

unclassified_is_woody_study2_confirmed <-
  unclassified_if_woody_taxa_study2_filled %>%
  # select on woody taxa from web search = 87 only
  filter(woodiness == "woody") %>%
  select(taxa)

# combine woody taxa identified using Simova list with identified by web search

data_taxa_all_woody <-
  bind_rows(woody_taxa_match, unclassified_is_woody_study2_confirmed) %>%
  distinct()

#----------------------------------------------------------#
# 5. Filter only woody taxa  ------------------
#----------------------------------------------------------#

data_study2_binned_renamed <-
  data_study2_binned %>%
  inner_join(neotoma_taxa, by = join_by("taxa"== "taxon_name")) %>%  # use neotoma taxa names for the data frame
  select(dataset_id, BIN, BIN_chr, summed_pollen_count, neotoma_names) %>% 
  rename(taxa = neotoma_names) %>% 
  relocate(taxa)

data_only_woody <-
  data_study2_binned_renamed %>%
  dplyr::inner_join(
    data_taxa_all_woody,
    by = "taxa"
  )

#----------------------------------------------------------#
# 6. Save reclassified harmonized taxa  ------------------
#----------------------------------------------------------#

write_csv(data_only_woody, here("Data/Paper_1/data_supplementary/data_only_woody.csv"))
