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
#         ---- ADD RANDOM SELECTION OF TIME  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <-
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

##rarefied data multiple iteration (1000x)

rarefied_dataset_assembly_asia <- 
  read_rds(here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_asia.rds"))

rarefied_dataset_assembly_europe <- 
  read_rds(here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_europe.rds"))

rarefied_dataset_assembly_namerica <- 
  read_rds(here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_namerica.rds"))

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
# 3. Add random selection of time  to each iteration ------
#----------------------------------------------------------#

##extract age uncertainties from full dataset

data_age_uncertainty <- 
  data %>% 
  select(dataset_id, age_uncertainty)

data_age_uncertainty %>% 
  distinct(dataset_id)

##show for a dataset id 

age_un_dataset_id <-
  data_age_uncertainty %>%
  filter(dataset_id =="1013") %>% 
  unnest(age_uncertainty)

# as a tibble
age_un_dataset_id_tib <-
  as_tibble(do.call(data.frame,age_un_dataset_id ))

#fix col names and create data frame with three cols(dataset_id, sample_id, potential_age)

dataset_potential_age <-
  rename_with(age_un_dataset_id_tib,
              ~ str_remove_all(.x,"[age_uncertainty.]")) %>% #rename cols to sample id
  rename(dataset_id = dsd) %>% 
  pivot_longer(!dataset_id, names_to = "sample_id", values_to = "potential_age")

dataset_potential_age %>% distinct(sample_id) # no of samples for each dataset_id

#iteration get potential ages from age_uncertainty for each dataset id

##Asia

### show rarefied data for all iteration(iter)

rarefied_dataset_assembly_asia_un <- 
  rarefied_dataset_assembly_asia %>% 
  unnest(rarefied_dataset) %>% 
  separate_wider_delim(dataset_id_age, delim = "_", names = c("dataset_id", "bin")) %>% 
  rename(iter = id) 

iter_1 <- 
  rarefied_dataset_assembly_asia_un %>% 
  filter(iter == "1") 

##Add age uncertainty to data assembly

rarefied_dataset_assembly_asia



get_potential_ages(data_age_uncertainty,"1013")

