#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 4: Bhatta et al
#                       
#                          2023
# Asia, site-based richness (dataset_id,age)
# nonbinned  - rarefy 300 
#
#
#              ----  DATA OVERVIEW ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data subsets ------------------------------------
#----------------------------------------------------------# 

pollen_data_study4 <- 
  read_rds( here("Data/Paper_1/data_subset/datasub_p1_s4_counts_ages.rds"))

data_study4_harmonised <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study4_harmonised.rds"))

data_rarefied_study4 <- 
  read_rds(here("Data/Paper_1/data_rarefy/data_study4_rarefied.rds"))

data_richness_study4 <- 
  read_csv(here("Data/Paper_1/data_estimate_richness/study4_richness.csv"))

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
  pollen_data_study4 %>% 
  get_number_of_datasets(group_var = NULL, name = "raw") %>% 
  rlang::set_names(
    nm = c( "n", "step")
  )

raw_n_samples <- 
  pollen_data_study4 %>% 
  get_number_of_samples(name = "raw", group_var = NULL)  %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

raw_n_taxa <- 
  pollen_data_study4 %>%
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
  data_study4_harmonised %>% 
  get_number_of_datasets(group_var = NULL, name = "harm") %>% 
  rlang::set_names(
    nm = c( "n", "step")
  )

harm_n_samples <- 
  data_study4_harmonised %>% 
  tidyr::unite("sample_id", c("dataset_id","age"), sep = "_", remove = FALSE ) %>% 
  get_number_of_samples(name = "harm", group_var = NULL)  %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

harm_n_taxa <- 
  data_study4_harmonised %>%
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
  data_rarefied_study4 %>% 
  tidyr::separate("dataset_id_age", c("dataset_id", "age"), sep = "_",remove = FALSE) %>%
  get_number_of_datasets(group_var = NULL, name = "rarefied") %>% 
  rlang::set_names(
    nm = c( "n", "step")
  )

rarefied_n_samples <- 
  data_rarefied_study4 %>% 
  dplyr::rename(sample_id = dataset_id_age) %>% 
  get_number_of_samples(name = "rarefied", group_var = NULL)  %>% 
  rlang::set_names(
    nm = c("n", "step")
  )

rarefied_n_taxa <- 
  data_rarefied_study4 %>%
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

#3.4.richness ----

richness_n_datasets <- 
  data_richness_study4 %>% 
  get_number_of_datasets(group_var = NULL, name = "richness") %>% 
  rlang::set_names(
    nm = c( "n", "step")
  )

richness_n_samples <- 
  data_richness_study4 %>% 
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

study4_data_overview <- 
  bind_rows(data_overview_raw, data_overview_harm,data_overview_rarefied,  data_overview_richness) %>% 
  mutate(study = "Study 4") %>% 
  relocate(study)


## Save data overview as dataframe

write_csv(study4_data_overview,here("Data/Paper_1/data_supplementary/data_overview/study4_data_overview.csv"))


#4. Plotting -----

all_data_overview_long <- 
  study4_data_overview %>% 
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

