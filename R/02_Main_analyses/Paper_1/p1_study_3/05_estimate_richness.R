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
#              ---- RICHNESS ESTIMATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_binned_asia_res_new <- 
  read_rds(here("Data/Paper_1/data_bin/data_binned_asia_res_iter.rds"))

rarefied_data_asia <- 
  read_rds(here("Data/Paper_1/data_rarefy/data_study3_rarefied_asia.rds"))

rarefied_data_europe <- 
read_rds(here("Data/Paper_1/data_rarefy/data_study3_rarefied_europe.rds"))

rarefied_data_namerica <- 
read_rds(here("Data/Paper_1/data_rarefy/data_study3_rarefied_namerica.rds"))

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
# 3. Estimate richness  at different taxo rank -- at 12 cal yr bp 
#----------------------------------------------------------# 

#3.1 Format rarefied data 

data_prepared_richness_estimation_asia <- 
  rarefied_data_asia %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = -c(dataset_id, BIN), 
               names_to = "taxa", values_to = "summed_pollen_count") %>% 
  prepare_data_for_richness_estimation(type = "binned")

data_prepared_richness_estimation_europe <- 
  rarefied_data_europe %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = -c(dataset_id, BIN), 
               names_to = "taxa", values_to = "summed_pollen_count") %>% 
  prepare_data_for_richness_estimation(type = "binned")

data_prepared_richness_estimation_namerica <- 
  rarefied_data_namerica %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = -c(dataset_id, BIN), 
               names_to = "taxa", values_to = "summed_pollen_count") %>% 
  prepare_data_for_richness_estimation(type = "binned")


### with iteration

####format to be accepted by prepare_data_for_richness_estimation(type = "binned")

data_binned_asia_res_new_for_richness_est <- 
  data_binned_asia_res_new %>% 
  mutate(summed_pollen_count = as.integer(summed_pollen_count)) %>% 
  pivot_wider(names_from = taxa, values_from = summed_pollen_count) %>% 
  select (!BIN_chr) %>% 
  unite("dataset_id_age", dataset_id, BIN, sep = "_", remove = TRUE) 

####prepare data for richness estimation

data_binned_asia_res_new_for_richness_est_new <- 
  data_binned_asia_res_new_for_richness_est %>% 
  separate_wider_delim(dataset_id_age, delim = "_", 
                       names = c("dataset_id","BIN")) %>% 
  pivot_longer(cols = -c(iter,dataset_id, BIN), 
               names_to = "taxa", values_to = "summed_pollen_count") 

#### runtime: ~ 1hr

iter <- 1:1000

data_binned_asia_res_richness_est_prep <- list()

for (i in iter){
  
  data_binned_asia_res_richness_est_prep[[i]] <- 
    data_binned_asia_res_new_for_richness_est_new %>% 
    filter(iter == i) %>% 
    prepare_data_for_richness_estimation(type = "binned")
  
}

write_rds(data_binned_asia_res_richness_est_prep,"Data/Paper_1/data_estimate_richness/data_binned_asia_res_richness_est_prep.rds" )
  
##transfrom back to a single dataframe

data_binned_asia_prep_new <- 
  bind_rows(data_binned_asia_res_richness_est_prep,.id = "iter")

write_rds(data_binned_asia_prep_new,"Data/Paper_1/data_estimate_richness/data_binned_asia_prep_new.rds" )

#3.2. Estimate richness

richness_asia <- 
  data_prepared_richness_estimation_asia %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(dataset_id = as_factor(dataset_id)) %>% 
  filter(age <= 12000)

summary(richness_asia)

richness_europe <- 
  data_prepared_richness_estimation_europe %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(dataset_id = as_factor(dataset_id)) %>% 
  filter(age <= 12000)

summary(richness_europe)


richness_namerica <- 
  data_prepared_richness_estimation_namerica %>% 
  estimate_richness() %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(dataset_id = as_factor(dataset_id)) %>% 
  filter(age <= 12000)

summary(richness_namerica)

##estimate richness with iteration;  run time:~ 2mins

data_estimate_richness_asia_res <- list()

for (i in iter){
  
  data_estimate_richness_asia_res[[i]] <- 
    data_binned_asia_prep_new %>% 
    filter(iter == i) %>% 
    estimate_richness() 

}

##transfrom back to a single dataframe

data_estimate_richness_asia_res_new <- 
  bind_rows(data_estimate_richness_asia_res,.id = "iter")

write_rds(data_estimate_richness_asia_res_new, here("Data/Paper_1/data_estimate_richness/study3_richness_asia_iter.rds"))

#----------------------------------------------------------#
# 4. Write the richness data to an RDS file
#----------------------------------------------------------# 

write_csv(richness_asia, here("Data/Paper_1/data_estimate_richness/study3_richness_asia.csv"))
write_csv(richness_europe, here("Data/Paper_1/data_estimate_richness/study3_richness_europe.csv"))
write_csv(richness_namerica, here("Data/Paper_1/data_estimate_richness/study3_richness_namerica.csv"))


list_richness <- 
  here::here("Data/Paper_1/data_rarefy/iterations") %>% 
  list.files(full.names = TRUE) %>% 
  purrr::map(
    .f = ~ readr::read_rds(.x) %>% 
      estimate_richness()
  )

