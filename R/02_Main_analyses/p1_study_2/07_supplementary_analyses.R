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
#               ---- SUBSETTING DATA  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(dplyr)
library(mgcv)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

data_p1_s2 <- data %>% 
  filter(region =="North America") %>%   # sub-setting data to N.America
  relocate(region) 



min(data_p1_s2$long)
max(data_p1_s2$long)
min(data_p1_s2$lat)
max(data_p1_s2$lat)


data_p1_s2 %>% 
  ggplot(aes(x=long, y= lat)) +
  borders(fill= NA, colour = "black") +
  geom_point(
  ) +
  coord_quickmap(xlim = c(-172, -53), ylim = c(28,74))


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

source_files <- sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

##### 3.2. get pollen counts with ages

data_p1_s2_counts_ages <- data_p1_s2  %>% get_pollen_counts_with_ages() 

#----------------------------------------------------------#
# 3. Test {harmonize_taxa} at different taxo rank --
#----------------------------------------------------------# 

harmonization_table  <- read_csv(here("Data/harmonization_table_rev.csv"), show_col_types = FALSE)
neotoma_taxa <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

taxa_level <- c("level_5", "level_6", "level_7") 
taxa_name <- c("family", "genus", "species")

# Harmonize taxa at different taxonomic levels

harmonized_data_study_2 <- purrr::map(taxa_level, ~ harmonize_taxa(data_p1_s2_counts_ages, data_ancillary, .x)) %>%
  set_names(taxa_name)


harmonized_data_study_2$genus

#####reclassify taxa (woody)

#option 1(priority) return a column woody (TRUE or FALSE)




#option 2
taxa_filtered_re <- taxa_filtered %>% rename(taxa = taxon_name)
harmonized_data_study_2$genus_re <- inner_join(harmonized_data_study_2$genus, taxa_filtered_re, by = "taxa")

#               ----  BINNING  ----
#----------------------------------------------------------#

#----------------------------------------------------------#
# 2. Bin data  at different taxo rank --
#----------------------------------------------------------# 

# Bin data

data_binned <-  harmonized_data_study_2$genus_re  %>% 
  mutate(
    BIN = cut(age, seq(min(age), 
                       max(age) + 1000, 1000), right = FALSE),
    BIN_chr = as.character(BIN),
    BIN_fct = as.factor(BIN_chr),
    BIN_int = as.factor(as.numeric(BIN_fct)), # recode BINS to integer, then factor) 
    BIN = BIN_int) %>% 
  group_by(dataset_id ,sample_id, taxa, BIN, BIN_chr) %>% 
  summarise(summed_pollen_count = sum(pollen_counts), .groups = "drop")


# Prepare data for richness estimation

prepared_data_for_richness_estimation <- data_binned %>% 
  rename(
    age = BIN,
    pollen_grains = summed_pollen_count
  ) %>% 
  select(dataset_id, sample_id, age, taxa, pollen_grains) %>% 
  filter(pollen_grains > 0) %>% 
  mutate(age = as.numeric(age)  * 1000) 


prepared_data_for_richness_estimation %>% summary() 

#                    ----RAREFACTION  ----
#----------------------------------------------------------#

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

prepared_data_for_richness_estimation %>% arrange(desc(age)) %>% head(10) # max. age

#----------------------------------------------------------#
# 2. Rarefy data  at different taxo rank --
#----------------------------------------------------------# 

all_samples <- unique(prepared_data_for_richness_estimation$sample_id)

# Apply rarefaction to all samples

results_list <- map(
  .x = all_samples,
  .f = ~ rarefy_pollen_grains_samp_iter(
    data_source = prepared_data_for_richness_estimation ,
    sample_id = .x,
    n_grains = 400,  # by 400
    n_iter = 1),
  .progress = "TRUE")

results <- dplyr::bind_rows(results_list) %>%  # rarefied richness
  as_tibble()

age <- prepared_data_for_richness_estimation %>% select(age, sample_id) # get age

results_age_rarefied <- left_join(results, age, by ="sample_id", multiple = "any") # add age col to rarefied richness

taxa <- results_age_rarefied  %>% distinct(taxon_name)

write_csv(taxa, here("Data/Processed/Other/p1_s2_taxa.csv")) # save raw taxa
taxa_filtered <- read_csv(here("Data/Processed/Other/p1_s2_taxa_filtered.csv"))

### join  results with filtered taxa based paper

results_age_filtered <- inner_join(results_age_rarefied,taxa_filtered , by = "taxon_name")

richness <- results_age_rarefied %>%    #  richness
  mutate(present = ifelse(avg_n_pollen_grains >= 1, 1, 0)) %>% 
  group_by(dataset_id,age, sample_id) %>% 
  summarize(richness = sum(present, na.rm = TRUE, .groups = NULL))


richness <- richness %>% filter(!is.na(age), !is.na(richness))
richness %>% summary()


richness <- richness %>%
  mutate(dataset_id = as_factor(dataset_id))

richness %>%
  ggplot(aes(x = age, y = richness)) +
  geom_smooth(method = "gam", color ="black") +
  scale_x_reverse(limits = c(20000,1000)) +
  geom_vline(xintercept = 9800, color ="red") +
  theme_classic()

# divide dataset to E-W cluster

cluster <- data_p1_s2 %>% select(dataset_id, long, lat) %>% 
  mutate(cluster = case_when(
    long < -104 ~ "West",
    long >= -104 ~ "East"))

data_richness_cluster <- inner_join(richness, cluster, by="dataset_id")



#View spatial distribution

data_richness_cluster %>% 
  ggplot(aes(x=long, y= lat, color = cluster)) +
  borders(fill= NA, colour = "black") +
  geom_point() +
  coord_quickmap(xlim = c(-172, -53), ylim = c(28,74))

# Plot richness by cluster

ggplot(data_richness_cluster, aes(x = age, y = richness, color = cluster, fill = cluster)) +
  geom_point()+
  geom_smooth(method = "gam", colour ="black") +
  scale_x_reverse(limits = c(20000, 1000), breaks = c(20000, 15000, 10000, 5000, 0), labels = c(20, 15, 10, 5, 0)) +
  scale_fill_manual(values = c("East" = "royalblue", "West" = "coral3")) +
  scale_color_manual(values = c("East" = "royalblue", "West" = "coral3")) +
  geom_vline(xintercept = 9800, color ="red") +
  theme_classic()

# plot richness by site

data_richness_cluster %>% 
  ggplot(aes(x = age, y = richness, group = dataset_id, color = long)) +
  geom_smooth(method = "gam",  formula = y ~ s(x, k=12),se = FALSE)+
  scale_x_reverse(limits = c(20000, 1000), breaks = c(20000, 15000, 10000, 5000, 0), labels = c(20, 15, 10, 5, 0)) + 
  theme_classic() +
  scale_color_gradient(high = "cadetblue1", low ="blue4")





library(furrr)

plan(multisession, workers=25)

results_list <- future_map(
  .x = all_samples,
  .f = ~ rarefy_pollen_grains_samp_iter(
    data_source = prepared_data_for_richness_estimation ,
    sample_id = .x,
    n_grains = 400,  # by 400
    n_iter = 1))

