#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 2: Simova et al
#
#                       
#                          2023
# North America, site-based richness (dataset_id,age, 
# 1000 bins - rarefy 400 
#
#              ---- RICHNESS ESTIMATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

rarefied_data <-
  read_rds(here("Data/Paper_1/data_rarefy/data_study2_rarefied.rds"))

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
# 3. Estimate richness  at different taxo rank ------------
#----------------------------------------------------------# 

## 3.1. Prepare data for richness_estimation

data_prepared_richness_estimation <- 
  rarefied_data  %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
   names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = -c(dataset_id, BIN), 
  names_to = "taxa", values_to = "summed_pollen_count") %>% 
  prepare_data_for_richness_estimation(type = "binned")



#Recheck if taxa is found in Simova list/ correct assignment of taxa based on this list (additional check)

data_prepared_richness_estimation_taxa <- 
  data_prepared_richness_estimation2 %>% distinct(taxa)

Not_is_simova_list <-
  read_csv(here("Data/Paper_1/data_supplementary/Not_is_simova_list.csv"))


data_prepared_richness_estimation2 <- 
  anti_join(data_prepared_richness_estimation,Not_is_simova_list, by = "taxa")


data_prepared_richness_estimation2 %>% filter(taxa == "Ephedra")

# some additional corrections with pollen-type to taxa conversion based on Simova et al 2023


data_prepared_richness_estimation2 <- 
  data_prepared_richness_estimation2 %>% 
  mutate(taxa = str_replace(taxa, "Ephedraceae", "Ephedra"),
         taxa = str_replace(taxa, "Ulmaceae", "Ulmus"),
         taxa = str_replace(taxa, "Sorbus", "Prunus/Sorbus"),
         taxa = str_replace(taxa, "Carpinus", "Ostrya/Carpinus"),
         taxa = str_replace(taxa, "Comptonia", "Myrica/Comptonia"),
         taxa = str_replace(taxa, "Thuja", "Juniperus/Thuja"),
         taxa = str_replace(taxa, "Toxicodendron", "Rhus/Toxicodendron"),
         taxa = str_replace(taxa, "Rhus", "Rhus/Toxicodendron"),
         taxa = str_replace(taxa, "Juniperus", "Juniperus/Thuja"),
         taxa = str_replace(taxa, "Prunus", "Prunus/Sorbus"),
         taxa = str_replace(taxa, "Ostrya", "Ostrya/Carnipus"),
         taxa = str_replace(taxa, "Myrica", "Myrica/Comptonia")
         )


richness <- 
  data_prepared_richness_estimation2 %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age))

summary(richness)

#----------------------------------------------------------#
# 4. Write the richness data to an RDS file ---------------
#----------------------------------------------------------#

write_csv(richness, here("Data/Paper_1/data_estimate_richness/study2_richness.csv"))
