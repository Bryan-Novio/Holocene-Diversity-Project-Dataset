
library(tidyverse)
library(here)
library(dplyr)

#----------------------------------------------------------#
# 1. Load dataset -----------------------------------------
#----------------------------------------------------------# 

harmonization_table_gen_final <- read_csv(here("Data/Processed/Data_harmonised/harmonization_table_gen_final.csv"), show_col_types = FALSE)

taxa_ref_table <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

prep_data_study_4 <- read_rds(here("Data/Processed/Other/prep_data_study_4.rds"))

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

sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

#----------------------------------------------------------#
# 3. Join datasets with neotoma class, ages, pollen grains  using  {join_data_prep_ages_pollen} 
#----------------------------------------------------------# 

prep_data_study_4_ages_pollen <- prep_data_study_4 |> 
  join_data_prep_ages_pollen()


#----------------------------------------------------------#
# 4. Test {harmonize_taxa} at different taxo rank --
#----------------------------------------------------------# 

harmonized_data_study_4_family <- harmonize_taxa(prep_data_study_4, prep_data_study_4_ages_pollen, "level_5")  
harmonized_data_study_4_genus <- harmonize_taxa(prep_data_study_4, prep_data_study_4_ages_pollen, "level_6")
harmonized_data_study_4_species <- harmonize_taxa(prep_data_study_4, prep_data_study_4_ages_pollen, "level_7")

#----------------------------------------------------------#
# 5. Bin (bin = 500), rarefy (n_grains = 500, n_iter = 10) 
#    and estimate richness harmonized dataset using {bin_rarefy_estimate_richness_harmonized_data }
#----------------------------------------------------------# 

s4_1 <- bin_rarefy_estimate_richness_harmonized_data(harmonized_data_study_4_family)    
s4_2 <- bin_rarefy_estimate_richness_harmonized_data(harmonized_data_study_4_genus)    
s4_3 <- bin_rarefy_estimate_richness_harmonized_data(harmonized_data_study_4_species)    

