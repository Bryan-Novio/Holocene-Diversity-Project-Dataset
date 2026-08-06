#----------------------------------------------------------#
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
#               ----  DATA OVERVIEW ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(patchwork)
library(tidytext)

#----------------------------------------------------------#
# 1. Load data subsets ------------------------------------
#----------------------------------------------------------# 

pollen_data_study2 <- 
  read_rds(here("Data/Paper_1/data_subset/datasub_p1_s2_counts_ages.rds"))

data_harmonised_study2 <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study2_harmonised.rds"))

data_rarefied_study2 <- 
  read_rds(here("Data/Paper_1/data_rarefy/data_study2_rarefied.rds"))

data_study2_binned <-
  read_rds(
    here("Data/Paper_1/data_bin/data_study2_binned.rds")
  )

data_richness_study2 <- 
  read_csv(here("Data/Paper_1/data_estimate_richness/study2_richness.csv"))

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
# 3. Data overview  -----------------
#----------------------------------------------------------#

#3.1.raw data ----

raw_n_datasets <- 
  pollen_data_study2 %>% 
  get_number_of_datasets(group_var = NULL, name = "raw") %>% 
  rlang::set_names(
    nm = c( "n", "step")
  )

raw_n_samples <- 
  pollen_data_study2 %>% 
  tidyr::unite("sample_id", c("dataset_id","age"), sep = "_", remove = FALSE ) %>% 
  get_number_of_samples(name = "raw", group_var = NULL)  %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

raw_n_taxa <- 
  pollen_data_study2 %>%
  get_number_of_taxa(name = "raw", group_var = NULL) %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

data_overview_raw <- 
  raw_n_datasets  %>% 
  dplyr::left_join(
    raw_n_samples,
    by = join_by(step),
    suffix = c("_datasets", "_samples")
  ) %>% 
  dplyr::left_join(
    raw_n_taxa %>% 
      dplyr::rename(
        n_taxa = n
      ),
    by = join_by(step),
  ) %>% 
  relocate(step, .before = n_datasets) 


#3.2 harmonized ----

harm_n_datasets <- 
  data_harmonised_study2 %>% 
  get_number_of_datasets(group_var = NULL, name = "harm") %>% 
  rlang::set_names(
    nm = c( "n", "step")
  )

harm_n_samples <- 
  data_harmonised_study2 %>% 
  tidyr::unite("sample_id", c("dataset_id","age"), sep = "_", remove = FALSE ) %>% 
  get_number_of_samples(name = "harm", group_var = NULL)  %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

harm_n_taxa <- 
  data_harmonised_study2 %>%
  dplyr::rename(taxa = taxon_name) %>% 
  get_number_of_taxa(name = "harm", group_var = NULL) %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

data_overview_harm <- 
  harm_n_datasets  %>% 
  dplyr::left_join(
    harm_n_samples,
    by = join_by(step),
    suffix = c("_datasets", "_samples")
  ) %>% 
  dplyr::left_join(
    harm_n_taxa %>% 
      dplyr::rename(
        n_taxa = n
      ),
    by = join_by(step),
  ) %>% 
  relocate(step, .before = n_datasets) 


# 3.3. rarefied ----

rarefied_n_datasets <- 
  data_rarefied_study2 %>% 
  tidyr::separate("dataset_id_age", c("dataset_id", "age"), sep = "_",remove = FALSE) %>%
  get_number_of_datasets(group_var = NULL, name = "rarefied") %>% 
  rlang::set_names(
    nm = c( "n", "step")
  )

rarefied_n_samples <- 
  data_rarefied_study2 %>% 
  dplyr::rename(sample_id = dataset_id_age) %>% 
  get_number_of_samples(name = "rarefied", group_var = NULL)  %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

rarefied_n_taxa <- 
  data_rarefied_study2 %>%
  pivot_longer(cols =  c(-dataset_id_age), values_to = "value", names_to = "taxa") %>% 
  get_number_of_taxa(name = "rarefied", group_var = NULL) %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

data_overview_rarefied <- 
  rarefied_n_datasets  %>% 
  dplyr::left_join(
    rarefied_n_samples,
    by = join_by(step),
    suffix = c("_datasets", "_samples")
  ) %>% 
  dplyr::left_join(
    rarefied_n_taxa %>% 
      dplyr::rename(
        n_taxa = n
      ),
    by = join_by(step),
  ) %>% 
  relocate(step, .before = n_datasets) 


#3.4. binned ----

binned_n_datasets <- 
  data_study2_binned %>% 
  get_number_of_datasets(group_var = NULL, name = "binned") %>% 
  rlang::set_names(
    nm = c( "n", "step")
  )

binned_n_samples <- 
  data_study2_binned %>% 
  tidyr::unite("sample_id", c("dataset_id","BIN"), sep = "_", remove = FALSE) %>% 
  get_number_of_samples(name = "binned", group_var = NULL)  %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

binned_n_taxa <- 
  data_study2_binned %>%
  get_number_of_taxa(name = "binned", group_var = NULL) %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

data_overview_binned <- 
  binned_n_datasets  %>% 
  dplyr::left_join(
    binned_n_samples,
    by = join_by(step),
    suffix = c("_datasets", "_samples")
  ) %>% 
  dplyr::left_join(
    binned_n_taxa %>% 
      dplyr::rename(
        n_taxa = n
      ),
    by = join_by(step),
  ) %>% 
  relocate(step, .before = n_datasets) 


#3.5.richness ----

richness_n_datasets <- 
  data_richness_study2 %>% 
  get_number_of_datasets(group_var = NULL, name = "richness") %>% 
  rlang::set_names(
    nm = c( "n", "step")
  )

richness_n_samples <- 
  data_richness_study2 %>% 
  tidyr::unite("sample_id", c("dataset_id","age"), sep = "_", remove = FALSE) %>% 
  get_number_of_samples(name = "richness", group_var = NULL)  %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

data_overview_richness <- 
  richness_n_datasets  %>% 
  dplyr::left_join(
    richness_n_samples,
    by = join_by(step),
    suffix = c("_datasets", "_samples")
  ) %>% 
  relocate(step, .before = n_datasets) 


#3.6. Merge all steps' overview

all_data_overview <- 
  bind_rows(data_overview_raw, data_overview_harm,data_overview_rarefied,  data_overview_binned,data_overview_richness )

#4. Plotting -----

all_data_overview_long <- 
  all_data_overview %>% 
  pivot_longer(
    cols = c(n_datasets,n_samples, n_taxa),
    names_to = "variable",
    values_to = "value"
    
  )


all_data_overview_long %>%  
  ggplot(aes(x = step, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  labs(
    x = "Step",
    y = "N",
    fill = "Metric"
  ) +
  theme_classic(
  )




