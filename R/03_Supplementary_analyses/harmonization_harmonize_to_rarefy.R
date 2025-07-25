
library(tidyverse)
library(here)
library(purrr)
library(dplyr)
library(ggpubr)

#----------------------------------------------------------#
# 1. Load dataset -----------------------------------------
#----------------------------------------------------------# 

harmonization_table_gen_final <- read_csv(here("Data/Processed/Data_harmonised/harmonization_table_gen_final.csv"), show_col_types = FALSE)

taxa_ref_table <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

prep_data_study <- list.files("Data/Processed/Other/", pattern ="[.]rds$", full.names = TRUE)

prep_data_study_list <- list(
  read_rds(here("Data/Processed/Other/prep_data_study_1.rds")),
  read_rds(here("Data/Processed/Other/prep_data_study_2.rds")),
  read_rds(here("Data/Processed/Other/prep_data_study_3.rds")),
  read_rds(here("Data/Processed/Other/prep_data_study_4.rds"))
  )

prep_data_study_list[[1]]

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
# 3. Join datasets with neotoma class, ages, pollen grains
#----------------------------------------------------------# 

prep_data_study_ages_pollen <- purrr::map(prep_data_study_list, join_data_prep_ages_pollen)
  
prep_data_study_ages_pollen[[1]]
#----------------------------------------------------------#
# 4. Create harmonize_taxa function -----------------------
#----------------------------------------------------------# 

harmonize_taxa <- function(data_to_harmonize, prep_data_study_ages_pollen , level) {
  taxa_level <- sym(level)  # Convert string to symbol for use in dplyr
  
  data_to_harmonize %>%
    pull(taxa) %>%
    unique() %>%
    tibble::tibble() %>%
    as_vector() %>%
    as_tibble() %>%
    rename(taxon_name = value) %>%
    inner_join(taxa_ref_table, by = "taxon_name") %>%
    select(neotoma_names) %>%
    rename(taxon_name = neotoma_names) %>%
    inner_join(harmonization_table_gen_final, by = "taxon_name") %>%
    select(taxon_name, level_5, level_6, level_7, level_8) %>%
    inner_join(prep_data_study_ages_pollen, by = "taxon_name", relationship = "many-to-many") %>%
    group_by(dataset_id, sample_id, age, !!taxa_level) %>%
    summarize(pollen_sum = sum(pollen_grains), .groups = "drop") %>% 
    rename(taxa = starts_with("level_")) %>% 
    rename(pollen_counts = pollen_sum) %>% 
    drop_na()
}

#----------------------------------------------------------#
# 5. Test harmonize_taxa function at different taxo rank --
#----------------------------------------------------------# 

# study dataset 1

harmonized_data_study_1_family <- harmonize_taxa(prep_data_study_list[[1]], prep_data_study_ages_pollen[[1]], "level_5")
harmonized_data_study_1_genus <- harmonize_taxa(prep_data_study_list[[1]], prep_data_study_ages_pollen[[1]], "level_6")
harmonized_data_study_1_species <- harmonize_taxa(prep_data_study_list[[1]], prep_data_study_ages_pollen[[1]], "level_7")

# study dataset 2

harmonized_data_study_2_family <- harmonize_taxa(prep_data_study_list[[2]], prep_data_study_ages_pollen[[2]], "level_5")
harmonized_data_study_2_genus <- harmonize_taxa(prep_data_study_list[[2]], prep_data_study_ages_pollen[[2]], "level_6")
harmonized_data_study_2_species <- harmonize_taxa(prep_data_study_list[[2]], prep_data_study_ages_pollen[[2]], "level_7") 

# study dataset 3

harmonized_data_study_3_family <- harmonize_taxa(prep_data_study_list[[3]], prep_data_study_ages_pollen[[3]], "level_5")
harmonized_data_study_3_genus <- harmonize_taxa(prep_data_study_list[[3]], prep_data_study_ages_pollen[[3]], "level_6")
harmonized_data_study_3_species <- harmonize_taxa(prep_data_study_list[[3]], prep_data_study_ages_pollen[[3]], "level_7")

# study dataset 4

harmonized_data_study_4_family <- harmonize_taxa(prep_data_study_list[[4]], prep_data_study_ages_pollen[[4]], "level_5")  
harmonized_data_study_4_genus <- harmonize_taxa(prep_data_study_list[[4]], prep_data_study_ages_pollen[[4]], "level_6")
harmonized_data_study_4_species <- harmonize_taxa(prep_data_study_list[[4]], prep_data_study_ages_pollen[[4]], "level_7")

#----------------------------------------------------------#
# 6. Bin (bin = 500), rarefy (n_grains = 500, n_iter = 10) 
#    and estimate richness harmonized dataset
#----------------------------------------------------------# 

#----------------------------------------------------------#
# study dataset 1
#----------------------------------------------------------#
    # at family level

s1_1 <- harmonized_data_study_1_family %>% 
  bin_data(1000) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10000) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

    # at genus level

s1_2 <- harmonized_data_study_1_genus %>% 
  bin_data(1000) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5,) +
  theme_classic()

    # at species level

s1_3 <- harmonized_data_study_1_species %>% 
  bin_data(1000) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.5,) +
  theme_classic()

#----------------------------------------------------------#
# study dataset 2
#----------------------------------------------------------#

    # at family level

s2_1 <- harmonized_data_study_2_family %>% 
  bin_data(500) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

# at genus level

s2_2 <- harmonized_data_study_2_genus %>% 
  bin_data(500) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

# at species level

s2_3 <- harmonized_data_study_2_species %>% 
  bin_data(500) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

#----------------------------------------------------------#
# study dataset 3
#----------------------------------------------------------#

# at family level

s3_1 <- harmonized_data_study_3_family %>% 
  bin_data(500) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

# at genus level

s3_2 <- harmonized_data_study_3_genus %>% 
  bin_data(500) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

# at species level

s3_3 <- harmonized_data_study_3_species %>% 
  bin_data(500) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

#----------------------------------------------------------#
# study dataset 4
#----------------------------------------------------------#

# at family level

s4_1 <- harmonized_data_study_4_family %>% 
  bin_data(500) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

# at genus level

s4_2 <- harmonized_data_study_4_genus %>% 
  bin_data(500) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

# at species level

s4_3 <- harmonized_data_study_4_species %>% 
  bin_data(500) %>%                      # binning 
  prepare_data_for_richness_estimation("binned") %>%  # prepare data for richness estimation
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(                            # rarefaction
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>%                             # estimate richness
  mutate(across(where(is.character), as.double)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()


#----------------------------------------------------------#
# 7. Plot all rarefied richness per dataset per taxo rank
#----------------------------------------------------------#

rarefied_richness_dataset_rank  <- ggarrange(s1_1, s1_2, s1_3,
                                             s2_1, s2_2, s2_3,
                                             s3_1, s3_2, s3_3,
                                             s4_1, s4_2, s4_3,
                labels = c("s1_f", "s1_g", "s1_s",
                           "s2_f", "s2_g", "s2_s",
                           "s3_f", "s3_g", "s3_s",
                           "s4_f", "s4_g", "s4_s"),
                ncol = 3, nrow = 4, hjust = -2.5, 
                font.label = list(size = 9, color ="red"))
