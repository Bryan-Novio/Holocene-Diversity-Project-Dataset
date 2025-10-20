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
#       ----  SELECT WOODY TAXA by criteria  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

library(usethis)
library(devtools)

library(DBI)
library(RPostgreSQL)

library(BIEN)
library(rtry)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_p1_s2_counts_ages <- read_rds(here("Outputs/Data/paper_1_study_2/datasub_p1_s2_counts_ages.rds"))

neotoma_taxa <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

#----------------------------------------------------------#
# 2. Apply criteria for woody taxa -------------------------
#----------------------------------------------------------#

# 2.1 Extract original taxa list from data set

taxa_orig <- data_p1_s2_counts_ages %>% select(taxa) %>%
  distinct() %>% arrange(taxa) %>% rename(taxon_name = taxa)

# 2.2 Join with neotoma taxa reference table to get max height and family/genus info

taxa_orig_neotoma <- inner_join(neotoma_taxa, taxa_orig, by = "taxon_name") %>% 
  rename(taxa = neotoma_names) %>%
  select(taxa)

# 2.3 Apply decision criteria to select woody taxa

#2.3.1. Obtain plant height from BIEN database

plant_height <- BIEN::BIEN_trait_trait(trait = "whole plant height") # load data frame instead of run

plant_height <- read_rds(here("Data/Processed/Other/bien_plant_height.rds"))

taxa_bien_height <- read_csv( here("Data/Processed/Other/taxa_bien_height.csv"))

taxa_bien_height <-  plant_height %>% 
  select(scrubbed_species_binomial, trait_name, trait_value, latitude, longitude) %>% 
  rename(taxa = scrubbed_species_binomial, height = trait_value) %>% 
  mutate(height = as.numeric(height)) %>%
  group_by(taxa) %>% 
  slice_max(order_by = height, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(taxa, height)

View(taxa_bien_height)

#2.3.2. Obtain plant height from TRY database - Plant height (TraitID 18)

TRYdataset_path <- here("Data/Processed/Other/TRY_plant_height_s2_p1.txt")

TRY_plant_height <- rtry_import(TRYdataset_path)


TRY_plant_height_explored <- rtry_explore(TRY_plant_height,
                                     AccSpeciesName, TraitName, StdValue, UnitName,
                                     sortBy =  AccSpeciesName)
View(TRY_plant_height_explored) 

TRY_plant_height_new <- TRY_plant_height_explored %>% 
  select(AccSpeciesName,StdValue) %>% 
  rename(taxa = AccSpeciesName, height = StdValue) 

TRY_plant_height_fix <- TRY_plant_height_new %>%    # change all to lowercase and then sentence case
  mutate(taxa = str_to_sentence(str_to_lower(taxa))) %>%
  drop_na() # remove rows with NA height

view(TRY_plant_height_fix)


all_taxa_height <- bind_rows(TRY_plant_height_fix, taxa_bien_height) %>% # taxa with height from BIEN (6922) + TRY (3606) = 10528
      arrange(taxa) 

all_taxa_height_databases <- all_taxa_height  %>% 
  group_by(taxa) %>% 
  slice_max(order_by = height, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(taxa, height) %>% 
  arrange(taxa)  # 8,190 taxa with height info retaining row with max value

write_csv(all_taxa_height_databases, here("Data/Processed/Other/all_taxa_height_databases.csv"))
semi_join(all_taxa_height_databases,un_taxa, by ="taxa") %>% filter(height > 1.5)





# Remove “cf.” from certain genera to search for height

taxa_orig_neotoma_cf <- taxa_orig_neotoma %>%
  mutate(
    taxa = if_else(
      str_detect(taxa, "^cf\\. "),
      str_replace(taxa, "^cf\\. ", ""),
      taxa
    )
  )  %>%
  print(n=1263)


taxa_orig_neotoma_databases <- left_join(taxa_orig_neotoma_cf,all_taxa_height_databases,by = "taxa") #146 taxa from dataset  with height info from BIEN/TRY

# Genera labeled with “cf.” were merged with the certain genera 

taxa_bien_height_crit_met <- taxa_bien_height %>% 
  filter(height >= 1.5) %>%  #  criteria of height >= 1.5 m
  mutate(genus = word(taxa, 1)) %>% # extract genus name
  filter(!str_detect(genus, "cf\\.")) %>% # remove "cf." taxa
  filter(!str_detect(genus, "Vitis|Smilax|Parthenocissus|Lonicera")) %>% # remove vines
  filter(!str_detect(taxa, "aceae")) %>% # remove families with >1 genera
  filter(!str_detect(taxa, "/")) %>% # remove aggregated uncertain taxa
  select(taxa, height) %>%
  arrange(taxa)


taxa_orig_neotoma_remain <- anti_join(taxa_orig_neotoma_cf, taxa_orig_neotoma_databases, by = "taxa") %>% arrange(taxa) # 98 taxa without height info from BIEN/TRY
View(taxa_orig_neotoma_remain)


taxa_orig_neotoma_remain_type <- taxa_orig_neotoma_remain %>% 
  separate(taxa, into = c("genus", "type"), sep = "-", fill = "right") %>% # clean taxa names with "-type"
  distinct(genus) %>%  # merge similar genus names
  select(genus) %>%
  rename(taxa = genus) 

                  
taxa_orig_neotoma_remain_type_species <- inner_join(taxa_orig_neotoma_remain_type, all_taxa_height_databases, by = "taxa")

taxa_orig_neotoma_remain_type_species_2 <- anti_join(taxa_orig_neotoma_remain_type,taxa_orig_neotoma_remain_type_species, by = "taxa")

View(taxa_orig_neotoma_remain_type_species_2) # 528 taxa remaining without height info at species level
# Further database search of taxa with  plant height for the remaining 1117 taxa at genus level

taxa_orig_neotoma_remain_type_genus <- taxa_orig_neotoma_remain_type_species_2  %>% # 867 remaining
  mutate(genus = taxa) %>% 
  select(genus) %>%
  distinct()

View(taxa_orig_neotoma_remain_type_genus)
 # 1013 unique genera

all_taxa_height_databases_genus <- all_taxa_height_databases %>%
  mutate(genus = taxa) %>% 
  select(genus, height) 


taxa_orig_neotoma_remain_type_genus_sep <- taxa_orig_neotoma_remain_type_genus %>% 
  separate(genus, into = c("genus", "type"), sep = " ", fill = "right") %>% 
  select(genus) %>%
  distinct()
 
all_taxa_height_databases_genus_sep <- all_taxa_height_databases_genus %>% 
  separate(genus, into = c("genus", "type"), sep = " ", fill = "right") %>% 
  group_by(genus) %>% 
  slice_max(order_by = height, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(genus, height) %>% 
  arrange(genus) 

  
matched_taxa <-  inner_join(taxa_orig_neotoma_remain_type_genus,all_taxa_height_databases_genus_sep, by = "genus") %>%
  select(genus, height) %>% 
  group_by(genus) %>% 
  slice_max(order_by = height, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  arrange(genus)



all_taxa_height_databases <- taxa_orig_neotoma_remain_type_genus %>% 
  inner_join(., matched_taxa,by = "genus") %>% 
  rename(taxa = genus) %>%# 555 other taxa with height at least at genus level
  select(taxa, height) %>%   # 696 taxa with height info from BIEN/TRY
  distinct(taxa) %>%
  arrange(taxa) 

# 2.3.3. Manual web search  for taxa without plant height from the databases
  
taxa_orig_neotoma_remain_2 <- taxa_orig_neotoma_remain_type_genus %>% 
  rename(taxa = genus) %>% 
  anti_join(all_taxa_height_databases, by = "taxa") %>%
  arrange(taxa) %>% 
  select(taxa)  # 421 taxa remaining without height info


View(taxa_orig_neotoma_remain_2)

1263 - 427 = 836


#----------------------------------------------------------#
# 3. Save outputs  ----------------------------------------
#----------------------------------------------------------#

write_csv(taxa_orig_neotoma, here("Data/Processed/Other/taxa_orig_neotoma.csv"))
write_rds(plant_height,here("Data/Processed/Other/bien_plant_height.rds"))
write_csv(taxa_bien_height, here("Data/Processed/Other/taxa_bien_height.csv"))
write_csv(taxa_bien_height_crit_met, here("Data/Processed/Other/taxa_bien_height_crit_met.csv"))
write_csv(taxa_bien_manual, here("Data/Processed/Other/taxa_bien_manual.csv"))
write_csv(taxa_orig_neotoma_remain, here("Data/Processed/Other/taxa_orig_neotoma_remain.csv"))
write_csv(taxa_orig_neotoma_remain_FINAL,here("Data/Processed/Other/taxa_orig_neotoma_remain_databases.csv"))


# ... (Previous code up to taxa_orig_neotoma_remain_type_genus)

all_taxa_height_databases_genus <- all_taxa_height_databases  %>%
  mutate(genus = taxa) %>%
  select(genus, height)
 

taxa_orig_neotoma_remain_type_genus_sep <- taxa_orig_neotoma_remain_type_genus %>%
  separate(genus, into = c("genus", "type"), sep = " ", fill = "right") %>%
  select(genus) %>%
  distinct()

all_taxa_height_databases_genus_sep <- all_taxa_height_databases_genus %>%
  separate(genus, into = c("genus", "type"), sep = " ", fill = "right") %>%
  group_by(genus) %>%
  slice_max(order_by = height, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(genus, height) %>%
  arrange(genus)
  
  
  
  


#### MODIFIED VERSION
  


#  Use the separated genus list to join with the height database at the genus level.
# This finds height for the original remaining genera.
matched_taxa_genus_level <- inner_join(taxa_orig_neotoma_remain_type_genus_sep,
                                       all_taxa_height_databases_genus_sep,
                                       by = "genus") %>%
  select(genus, height) %>%

  group_by(genus) %>%
  slice_max(order_by = height, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(genus) %>%
  rename(taxa = genus) %>%
  select(taxa, height)

# -------------------------------------------------------------------------------- #
# NEW STEP: Identify the set of taxa from 'taxa_orig_neotoma' that now have height.
# -------------------------------------------------------------------------------- #

# Start with the 'taxa_orig_neotoma_databases' (taxa with height at species level)
taxa_with_height_species_level <- taxa_orig_neotoma_databases %>%
  select(taxa) %>%
  distinct()


# Get the original full list of taxa that had no height info (the "remain" list)
taxa_orig_neotoma_remain_full <- anti_join(taxa_orig_neotoma, taxa_orig_neotoma_databases, by = "taxa") %>%
  arrange(taxa) 


# For the 'remain' list, check if their *genus* is in the 'matched_taxa_genus_level'
taxa_with_height_genus_level <- taxa_orig_neotoma_remain_full %>%

  separate(taxa, into = c("genus", "rest"), sep = "[ -]", extra = "drop", fill = "right", remove = FALSE) %>%
  inner_join(matched_taxa_genus_level, by = c("genus" = "taxa")) %>%
  select(taxa) %>% 
  distinct()



# Combine ALL taxa from 'taxa_orig_neotoma' that now have height info
all_taxa_with_height <- bind_rows(taxa_with_height_species_level,
                                  taxa_with_height_genus_level) %>%
  distinct(taxa)




# Final Anti-join: Return the least number of taxa from 'taxa_orig_neotoma'

taxa_orig_neotoma_remain_FINAL <- anti_join(taxa_orig_neotoma, all_taxa_with_height, by = "taxa") %>%
  arrange(taxa) %>%
  select(taxa)

View(taxa_orig_neotoma_remain_FINAL)
taxa_orig_neotoma_remain_FINAL

