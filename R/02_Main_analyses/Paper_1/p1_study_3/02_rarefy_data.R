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

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <-
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

data_binned_asia <- 
  read_rds(here("Data/Paper_1/data_bin/data_study3_binned_asia.rds"))

data_binned_europe <- 
  read_rds(here("Data/Paper_1/data_bin/data_study3_binned_europe.rds"))

data_binned_namerica <- 
  read_rds(here("Data/Paper_1/data_bin/data_study3_binned_namerica.rds"))

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
# 3. Rarefy data  at different taxo rank --
#----------------------------------------------------------# 

##Asia

data_to_rarefy_asia <- 
  data_binned_asia %>% 
  inner_join(neotoma_taxa, join_by(taxa == neotoma_names)) %>% 
  select(dataset_id, BIN, taxa,summed_pollen_count) %>% 
  rename(age = BIN, pollen_counts = summed_pollen_count, taxon_name = taxa) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )
  
set.seed(1234) # do not run if will do repeated rarefaction

rarefied_data_asia <-
  rarefy_all_samples(
    data_source = data_to_rarefy_asia,
    n_grains = 300
  )

# do repetitive rarefaction 1000 times

rarefied_dataset_assembly_asia <- 
  data_to_rarefy_asia %>% 
  rarefy_all_samples_iter(n_iter = 1000)


rarefied_dataset_assembly_asia %>% filter(id == "1") %>% unnest(rarefied_dataset)

## Europe

data_to_rarefy_europe <- 
  data_binned_europe %>% 
  inner_join(neotoma_taxa, join_by(taxa == neotoma_names)) %>% 
  select(dataset_id, BIN, taxa,summed_pollen_count) %>% 
  rename(age = BIN, pollen_counts = summed_pollen_count, taxon_name = taxa) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )

set.seed(1234)

rarefied_data_europe <-
  rarefy_all_samples(
    data_source = data_to_rarefy_europe,
    n_grains = 300)

##with iteration 1000x

rarefied_dataset_assembly_europe <- 
  data_to_rarefy_europe%>% 
  rarefy_all_samples_iter(n_iter = 1000)

##NAmerica

data_to_rarefy_namerica <- 
  data_binned_namerica %>% 
  inner_join(neotoma_taxa, join_by(taxa == neotoma_names)) %>% 
  select(dataset_id, BIN, taxa,summed_pollen_count) %>% 
  rename(age = BIN, pollen_counts = summed_pollen_count, taxon_name = taxa) %>% 
  mutate(pollen_counts = as.integer(round(pollen_counts, digits = 0)) # ensure pollen_counts are integers(required in 'rarefy' in vegan)
  )  %>% 
  pivot_wider(
    names_from = taxon_name,
    values_from = pollen_counts
  )

set.seed(1234)

rarefied_data_namerica <-
  rarefy_all_samples(
    data_source = data_to_rarefy_namerica,
    n_grains = 300)

##with iteration 1000x

rarefied_dataset_assembly_namerica <- 
  data_to_rarefy_namerica %>% 
  rarefy_all_samples_iter(n_iter = 1000)


#----------------------------------------------------------#
# 4. Add random selection of time  to each iteration ------
#----------------------------------------------------------#

##extract age uncertainties from full dataset

data_age_uncertainty <- 
  data %>% 
  select(dataset_id, age_uncertainty)

data_age_uncertainty %>% 
  filter(dataset_id =="1001") %>% 
  unnest(age_uncertainty)

data_potential_ages <- 
  get_potential_ages(data_age_uncertainty) #using a function

## test rarefied data with iterations

rarefied_dataset_assembly_asia <- 
  read_rds(here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_asia.rds")) %>% rename(iter  = id)

rarefied_dataset_assembly_europe <- 
  read_rds(here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_europe.rds")) %>% rename(iter  = id)

rarefied_dataset_assembly_namerica <- 
  read_rds(here("Data/Paper_1/data_rarefy/rarefied_dataset_assembly_namerica.rds")) %>% rename(iter  = id)

# collapse rarefied data assembly to single dataframe

rarefied_dataset_assembly_asia_un <-
  rarefied_dataset_assembly_asia %>%
  unnest(cols = c(rarefied_dataset)) %>% 
  separate_wider_delim(.,cols = dataset_id_age,
                       names = c("dataset_id", "bin"),delim ="_") %>% 
  select(-bin)

rarefied_dataset_assembly_europe_un <-
  rarefied_dataset_assembly_europe %>%
  unnest(cols = c(rarefied_dataset)) %>% 
  separate_wider_delim(.,cols = dataset_id_age,
                       names = c("dataset_id", "bin"),delim ="_") %>% 
  select(-bin)

rarefied_dataset_assembly_namerica_un <-
  rarefied_dataset_assembly_namerica %>%
  unnest(cols = c(rarefied_dataset)) %>% 
  separate_wider_delim(.,cols = dataset_id_age,
                       names = c("dataset_id", "bin"),delim ="_") %>% 
  select(-bin)

# Add random selection of time to rarefied dataset with iterations

##dataset_ids from assembly

dataset_assembly_asia_ids <-                 
  rarefied_dataset_assembly_asia_un %>% 
  distinct(dataset_id) %>% 
  as.vector()

dataset_assembly_europe_ids <-                 
  rarefied_dataset_assembly_europe_un %>% 
  distinct(dataset_id) %>% 
  as.vector()

dataset_assembly_namerica_ids <-                 
  rarefied_dataset_assembly_namerica_un %>% 
  distinct(dataset_id) %>% 
  as.vector()

## filter potential ages for dataset ids in the assembly

data_potential_ages_filtered_asia <- 
  data_potential_ages %>%
  filter(dataset_id %in% dataset_assembly_asia_ids$dataset_id)

data_potential_ages_filtered_europe <- 
  data_potential_ages %>%
  filter(dataset_id %in% dataset_assembly_europe_ids$dataset_id)

data_potential_ages_filtered_namerica <- 
  data_potential_ages %>%
  filter(dataset_id %in% dataset_assembly_namerica_ids$dataset_id)

## add random ages for each dataset_id in the assembly

rarefied_dataset_assembly_asia_p_ages <- 
  purrr::map(dataset_assembly_asia_ids$dataset_id, 
             ~ add_random_ages(., rarefied_dataset_assembly_asia_un,   data_potential_ages_filtered_asia)) %>% 
  list_rbind() %>% 
  mutate(potential_age  = as.double(potential_age))


rarefied_dataset_assembly_europe_p_ages <- 
  purrr::map(dataset_assembly_europe_ids$dataset_id, 
             ~ add_random_ages(., rarefied_dataset_assembly_europe_un,   data_potential_ages_filtered_europe)) %>% 
  list_rbind() %>% 
  mutate(potential_age  = as.double(potential_age))

rarefied_dataset_assembly_namerica_p_ages <- 
  purrr::map(dataset_assembly_namerica_ids$dataset_id, 
             ~ add_random_ages(., rarefied_dataset_assembly_namerica_un,   data_potential_ages_filtered_namerica)) %>% 
  list_rbind() %>% 
  mutate(potential_age  = as.double(potential_age))

## transform data for binning

rarefied_dataset_assembly_asia_p_ages_to_bin <- 
  rarefied_dataset_assembly_asia_p_ages %>%
  pivot_longer(cols = -c(potential_age,iter,dataset_id),
               names_to = "taxa",
               values_to = "pollen_counts") %>% 
  rename(age = potential_age) %>% 
  mutate(pollen_counts  = as.double(pollen_counts))

rarefied_dataset_assembly_europe_p_ages_to_bin <-       ##do not run -- Error in `vec_interleave_indices()`: vector lim reached
  rarefied_dataset_assembly_europe_p_ages %>%
  pivot_longer(cols = -c(potential_age,iter,dataset_id),
               names_to = "taxa",
               values_to = "pollen_counts") %>% 
  rename(age = potential_age) %>% 
  mutate(pollen_counts  = as.double(pollen_counts))

rarefied_dataset_assembly_namerica_p_ages_to_bin <- 
  rarefied_dataset_assembly_namerica_p_ages %>%
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

##rarefied data single iteration

write_rds(rarefied_data_asia, here("Data/Paper_1/data_rarefy/data_study3_rarefied_asia.rds"))

write_rds(rarefied_data_europe, here("Data/Paper_1/data_rarefy/data_study3_rarefied_europe.rds"))

write_rds(rarefied_data_namerica, here("Data/Paper_1/data_rarefy/data_study3_rarefied_namerica.rds"))


