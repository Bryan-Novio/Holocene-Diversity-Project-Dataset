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

data_binned_filtered <-
  read_rds(
    here("Outputs/Data/paper_1_study_2/data_binned_filtered.rds")
  )

# This is as supplementary file from DOI: 10.1111/geb.13649 

woody_taxa <- read_csv("Data/Processed/Other/woody_taxa_simova.csv", show_col_types = FALSE)


neotoma_taxa <- readr::read_csv(
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

source_files <- sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)


#----------------------------------------------------------#
# 2. Reclassify harmonized dataset using woody taxa list ---
#----------------------------------------------------------#

woody_taxa_re <- woody_taxa %>%
  rename(taxa = "Taxon name orig.") %>%
  select(taxa)

View(woody_taxa_re)

woody_taxa_re %>% unique()

# Extract all unique taxa from rawdataset

data_binned_filtered_taxa <- data_binned_filtered %>%
  distinct(taxa)

study_2_taxa <- inner_join(
  data_binned_filtered_taxa,
  neotoma_taxa,
  by = join_by("taxa" == "taxon_name")
) %>%
  distinct(neotoma_names)


woody_taxa_match <- inner_join(
  woody_taxa_re,
  study_2_taxa,
  by = join_by("taxa" == "neotoma_names")
)

# -> It now has 134 woody match


un_taxa <- anti_join(
  study_2_taxa,
  woody_taxa,
  by = join_by("neotoma_names" == 'Taxon name orig.')) %>% 
  rename(taxa = neotoma_names) %>% 
  distinct(taxa) # 245 unclassified taxa

# save the unmatched taxa

write_csv(un_taxa, here("Data/Processed/Other/unclass_taxa.csv"))

#----------------------------------------------------------#
# 3. Manual search for the unmatched taxa  ------------------
#----------------------------------------------------------#

# We have individually assing the unmatched taxa and
#   saved as `unclass_taxa_filled.csv`

#----------------------------------------------------------#
# 4. Manual search for the unmatched taxa  ------------------
#----------------------------------------------------------#

# read now classified taxa based on web search # 242 only because of duplicate of (cf.) Oxyria digyna

un_taxa_re <- read_csv(here("Data/Processed/Other/unclass_taxa_filled.csv"))

un_taxa_re_woody <-
  un_taxa_re %>%
  # select on woody taxa from web search = 87 only
  filter(woodiness == "woody") %>%
  select(taxa)

# combine woody taxa identified using Simova list with identified by web search

data_taxa_all_woody <-
  bind_rows(woody_taxa_match, un_taxa_re_woody) %>%
  distinct()

#----------------------------------------------------------#
# 5. Filter only woody taxa  ------------------
#----------------------------------------------------------#

data_binned_filtered_re <- data_binned_filtered %>%
  inner_join(neotoma_taxa, by = join_by("taxa"== "taxon_name")) %>%  # use neotoma taxa names for the data frame
  select(dataset_id, BIN, BIN_chr, summed_pollen_count, neotoma_names) %>% 
  rename(taxa = neotoma_names) %>% 
  relocate(taxa)

data_only_woody <-
  data_binned_filtered_re %>%
  dplyr::inner_join(
    data_taxa_all_woody,
    by = "taxa"
  )

#----------------------------------------------------------#
# 6. Save reclassified harmonized taxa  ------------------
#----------------------------------------------------------#

write_csv(data_only_woody, here("Data/Processed/Other/data_only_woody.csv"))
