#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 3: Gordon et al
#
#                       
#                          2024
#
# North America, site-based richness (dataset_id,age, 
# 500 bins - rarefy 300 
#
#                 ----RAREFACTION  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(vegan)
library(tictoc)
library(furrr)
library(waldo)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------#

data_harmonised_merge <-
  read_rds(here("Data/Paper_1/data_harmonize/data_study3_data_harmonised_merge.rds"))

data_age_uncertainty <- 
  read_rds(here("Data/Paper_1/data_subset/data_age_uncertainty.rds"))

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
# 3. Rarefy data  fo 1000 iterations --
#----------------------------------------------------------#

## 3.1.Prepare data for rarefaction

data_to_rarefy <-   # 1001 dataset_ids
  data_harmonised_merge %>% 
  select(dataset_id, age, taxa, pollen_counts ) %>% 
  rename(taxon_name = taxa) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )

## 3.2. Rarefy iteratively


tic()

set.seed(12345)

rarefied_dataset_assembly <- 
  data_to_rarefy %>% 
  rarefy_all_samples_iter(n_iter = 1000) %>% 
  tidyr::nest(
    rarefied_dataset = -c(id)
  )
toc()

## 3.3. Check rarefied datasets

manual_check <- FALSE

if (
 manual_check == TRUE  
) {
  
  waldo::compare(
    rarefied_dataset_assembly$rarefied_dataset[[1]],
    rarefied_dataset_assembly$rarefied_dataset[[2]]
  )
  
  rarefied_dataset_assembly%>% filter(id == "1") %>% unnest(rarefied_dataset)
}



## transform data for binning

rarefied_dataset_assembly_asia_p_ages_to_bin <- 
  rarefied_dataset_assembly_asia_p_ages %>%
  pivot_longer(cols = -c(potential_age,iter,dataset_id),
               names_to = "taxa",
               values_to = "pollen_counts") %>% 
  rename(age = potential_age) %>% 
  mutate(pollen_counts  = as.double(pollen_counts))



#----------------------------------------------------------#
# 4. Write the rarefied data to an RDS file --------------
#----------------------------------------------------------#

##rarefied data multiple iteration (1000x)

write_rds(rarefied_dataset_assembly_asia, here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_asia.rds"))

write_rds(rarefied_dataset_assembly_europe, here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_europe.rds"))

write_rds(rarefied_dataset_assembly_namerica, here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_namerica.rds"))


## rarefied_data_assembly for binning

write_rds(rarefied_dataset_assembly_asia_p_ages_to_bin, here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_asia_p_ages_to_bin.rds"))

write_rds(rarefied_dataset_assembly_europe_p_ages_to_bin, here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_europe_p_ages_to_bin.rds"))

write_rds(rarefied_dataset_assembly_namerica_p_ages_to_bin, here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_namerica_p_ages_to_bin.rds"))


