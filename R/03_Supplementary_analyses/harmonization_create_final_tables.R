library(tidyverse)
library(here)
library(purrr)
library(dplyr)

#----------------------------------------------------------#
# 1. Load up taxon list per study, classified taxon files
#----------------------------------------------------------# 

method_data_files    <- list.files("Data/Processed/Other/", pattern ="[.]rds$", full.names = TRUE)
class_taxa_success   <- read_rds(here("Data/Processed/Data_harmonised/class_taxa_succeeded.rds"))
class_taxa_plants    <- read_csv(here("Data/Processed/Data_harmonised/class_taxa_plants.csv"), show_col_types = FALSE)
taxa_ref_table       <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

#----------------------------------------------------------#
# 2. Extract taxon list from each study
#----------------------------------------------------------#

method_data_files


method_data <- purrr::map(method_data_files,readr::read_rds) 
method_data_taxa <- purrr::map(method_data, ~ pull(.x, taxa))  # extract taxon per study
str(method_data_taxa)

method_data_taxa[1]

method_data_taxa_unique <- purrr::map(method_data_taxa, ~ unique(.x)) # select only unique taxa per taxon list 
str(method_data_taxa_unique[[1]])
str(method_data_taxa_unique[[2]])
str(method_data_taxa_unique[[3]])
str(method_data_taxa_unique[[4]])

method_data_taxa_unique_tib <- tibble::tibble(method_data_taxa_unique) # comment
str(method_data_taxa_unique_tib)

method_data_taxa_unique_tib_1 <- method_data_taxa_unique_tib[[1]][[1]] %>% as_vector() %>% as_tibble() %>% rename(taxon_name = value) # extract taxon list per study as tibble 
method_data_taxa_unique_tib_2 <- method_data_taxa_unique_tib[[1]][[2]] %>% as_vector() %>% as_tibble() %>% rename(taxon_name = value)
method_data_taxa_unique_tib_3 <- method_data_taxa_unique_tib[[1]][[3]] %>% as_vector() %>% as_tibble() %>% rename(taxon_name = value)
method_data_taxa_unique_tib_4 <- method_data_taxa_unique_tib[[1]][[4]] %>% as_vector() %>% as_tibble() %>% rename(taxon_name = value)


method_data_taxa_unique_join_neotoma_1 <- inner_join(method_data_taxa_unique_tib_1, taxa_ref_table, by ='taxon_name') %>% select(neotoma_names) %>% rename(taxon_name = neotoma_names) # translate each taxa to neotoma name
method_data_taxa_unique_join_neotoma_2 <- inner_join(method_data_taxa_unique_tib_2, taxa_ref_table, by ='taxon_name') %>% select(neotoma_names) %>% rename(taxon_name = neotoma_names)
method_data_taxa_unique_join_neotoma_3 <- inner_join(method_data_taxa_unique_tib_3, taxa_ref_table, by ='taxon_name') %>% select(neotoma_names) %>% rename(taxon_name = neotoma_names)
method_data_taxa_unique_join_neotoma_4 <- inner_join(method_data_taxa_unique_tib_4, taxa_ref_table, by ='taxon_name') %>% select(neotoma_names) %>% rename(taxon_name = neotoma_names)


#----------------------------------------------------------#
# 3. Create general harmonization table 
#----------------------------------------------------------#

class_taxa_hlist <- class_taxa_success %>% 
  select(sel_name,classification) %>% 
  inner_join(class_taxa_plants, by = "sel_name") %>%            
  select(sel_name, classification) %>% 
  unnest_wider(classification) %>% 
  select(sel_name, name, rank)

class_taxa_hlist_level <- class_taxa_hlist %>% 
  select(sel_name, rank) %>% 
  unnest(rank) %>% 
  distinct(rank)

class_taxa_hlist_init <- class_taxa_hlist %>% 
  select(sel_name, name) %>% 
  unnest_wider(name, names_sep = "_" ) 

class_taxa_hlist_final <-  class_taxa_hlist_init %>% 
  rename_with(.fn = ~ str_replace(.x,"name_", "level_"), 
              .cols = starts_with("name_")
  ) 

harmonization_table_gen <- class_taxa_hlist_final %>% rename(taxon_name = sel_name) #

harmonization_table_gen_combined <- bind_rows(harmonization_table_gen, class_taxa_100) #

write_csv(harmonization_table_gen, here("Data/Processed/Data_harmonised/harmonization_table_gen.csv"))
write_csv(harmonization_table_gen_final, here("Data/Processed/Data_harmonised/harmonization_table_gen_final.csv"))
#----------------------------------------------------------#
# 4. Create harmonization table per study
#----------------------------------------------------------# 

harmonization_table_s1 <- inner_join(method_data_taxa_unique_join_neotoma_1, harmonization_table_gen, by = "taxon_name")
harmonization_table_s2 <- inner_join(method_data_taxa_unique_join_neotoma_2, harmonization_table_gen, by = "taxon_name")
harmonization_table_s3 <- inner_join(method_data_taxa_unique_join_neotoma_3, harmonization_table_gen, by = "taxon_name")
harmonization_table_s4 <- inner_join(method_data_taxa_unique_join_neotoma_4, harmonization_table_gen, by = "taxon_name")

harmonization_table_s1_re <- inner_join(method_data_taxa_unique_join_neotoma_1, harmonization_table_gen_final, by = "taxon_name")
harmonization_table_s2_re <- inner_join(method_data_taxa_unique_join_neotoma_2, harmonization_table_gen_final, by = "taxon_name")
harmonization_table_s3_re <- inner_join(method_data_taxa_unique_join_neotoma_3, harmonization_table_gen_final, by = "taxon_name")
harmonization_table_s4_re <- inner_join(method_data_taxa_unique_join_neotoma_4, harmonization_table_gen_final, by = "taxon_name")

harmonization_table_s1_re %>% select(level_5,level_6, level_7, level_8)
harmonization_table_s2_re %>% select(level_5,level_6, level_7, level_8)
harmonization_table_s3_re %>% select(level_5,level_6, level_7, level_8)
harmonization_table_s4_re %>% select(level_5,level_6, level_7, level_8)
  











####================================================ harmonize function


library(tidyverse)
library(here)
library(purrr)
library(dplyr)

#----------------------------------------------------------#
# 1. Load dataset
#----------------------------------------------------------# 

harmonization_table_gen_final <- read_csv(here("Data/Processed/Data_harmonised/harmonization_table_gen_final.csv"), show_col_types = FALSE)
taxa_ref_table <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)
prep_data_study_1 <- read_rds(here("Data/Processed/Other/prep_data_study_1.rds"))
prep_data_study_4 <- read_rds(here("Data/Processed/Other/prep_data_study_4.rds"))

#----------------------------------------------------------#
# 2. Create harmonize_taxa function
#----------------------------------------------------------# 

# abridged version

harmonize_2 <- function(data_to_harmonize) {
  data_to_harmonize %>% 
    pull( taxa) %>%
    unique() %>% 
    tibble::tibble() %>% 
    as_vector() %>% as_tibble() %>% rename(taxon_name = value) %>% 
    inner_join(taxa_ref_table, by ='taxon_name') %>% 
    select(neotoma_names) %>% 
    rename(taxon_name = neotoma_names) %>% 
    inner_join(harmonization_table_gen_final, by = "taxon_name") %>% 
    select(taxon_name, level_5,level_6, level_7, level_8)
}

#----------------------------------------------------------#
# 3. Test harmonize_taxa function
#----------------------------------------------------------# 

harmonize_my_taxa <- harmonize(prep_data_study_1)
harmonize_my_taxa <- harmonize(prep_data_study_4)

harmonize_my_taxa_2_1 <- harmonize_2(prep_data_study_1, prep_data_study_1_re)
harmonize_my_taxa_2_4 <- harmonize_2(prep_data_study_4, prep_data_study_4_re)

#================= revision of harmonization function ##

# join raw study dataset with neotoma classification before harmonization

prep_data_study_4_re <- prep_data_study_4 %>% rename(taxon_name = taxa) %>% 
  inner_join(taxa_ref_table, by = "taxon_name") %>% 
  select(dataset_id, age, pollen_grains, sample_id, neotoma_names) %>% 
  rename(taxon_name = neotoma_names)

prep_data_study_1_re <- prep_data_study_1 %>% rename(taxon_name = taxa) %>% 
  inner_join(taxa_ref_table, by = "taxon_name") %>% 
  select(dataset_id, age, pollen_grains, sample_id, neotoma_names) %>% 
  rename(taxon_name = neotoma_names)

# revise harmonization function


harmonize_taxa <- function(data_to_harmonize, renamed_data, level) {
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
    inner_join(renamed_data, by = "taxon_name", relationship = "many-to-many") %>%
    group_by(dataset_id, sample_id, age, !!taxa_level) %>%
    summarize(pollen_sum = sum(pollen_grains), .groups = "drop")
}


harmonized_data_study_1_fam <- harmonize_taxa(prep_data_study_1, prep_data_study_1_re, "level_5")
harmonize_taxa(prep_data_study_1, prep_data_study_1_re, "level_6")
harmonize_taxa(prep_data_study_1, prep_data_study_1_re, "level_7")

harmonize_taxa(prep_data_study_4, prep_data_study_4_re, "level_5")
harmonize_taxa(prep_data_study_4, prep_data_study_4_re, "level_6")
harmonize_taxa(prep_data_study_4, prep_data_study_4_re, "level_7")


data_binned_500 <- 
  get_pollen_counts_with_ages(data) %>% 
  bin_data(., 500) 


data %>% 
  select(dataset_id, raw_counts) %>% 
  unnest(raw_counts) %>% 
  pivot_longer(
    cols = !c(dataset_id,sample_id),
    names_to = "taxa", values_to = "pollen_counts",
    values_drop_na = TRUE)



harmonized_data_study_1_fam_re <- harmonized_data_study_1_fam %>%  rename(taxa = level_5)
harmonized_data_study_1_fam_re_1 <- harmonized_data_study_1_fam_re %>%  rename(pollen_counts = pollen_sum)



#----------------------------------------------------------#
# 1. Load functions ---------------------------------------
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



harmonized_data_study_1_binned <- bin_data(harmonized_data_study_1_fam_re_1, 500)  # skip get_pollen_counts_with_ages

harmonized_data_study_1_for_richness <- prepare_data_for_richness_estimation(harmonized_data_study_1_binned, "binned")


harmonized_data_study_1_for_estimate_richness <-  estimate_richness(data_for_richness_estimation_binned_500)


harmonized_data_study_1_fam_re_1 %>% 
  bin_data(bin_size = 500) %>% 
  prepare_data_for_richness_estimation(type = "binned") %>% 
  estimate_richness()


rarefied_harmonized_data <- harmonized_data_study_1_fam_re_1 %>% 
  bin_data(bin_size = 500) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 500,
    n_iter = 10) 

rarefied_harmonized_data %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>% 
  
  ggplot(aes(y = richness, x =  age)) + 
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()
