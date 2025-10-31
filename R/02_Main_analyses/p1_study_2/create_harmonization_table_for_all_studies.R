#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                      ALL STUDIES
#
#       ---- CREATE CUSTOM HARMONIZATION TABLE ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(purrr)
library(dplyr)
library(taxospace)

#STEP 1: CLASSIFY STUDY TAXA

#----------------------------------------------------------#
# 1. Load data files per dataset (study)------------------
#----------------------------------------------------------#

p1_s1_datasubset <- read_rds(here("Outputs/Data/paper_1_study_1/datasub_p1_s1_counts_ages.rds"))
p1_s2_datasubset <- read_rds(here("Outputs/Data/paper_1_study_2/datasub_p1_s2_counts_ages.rds"))
p1_s3_datasubset <- read_rds(here("Outputs/Data/paper_1_study_3/datasub_p1_s3_na_counts_ages.rds"))
p1_s4_datasubset <- read_rds(here("Outputs/Data/paper_1_study_4/datasub_p1_s4_counts_ages.rds"))

datasubset_all  <- list.files("Outputs/Data/p1_datasubset/", pattern ="[.]rds$", full.names = TRUE)

taxa_ref_table <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

#----------------------------------------------------------#
# 2. Select unique taxa from all dataset from each study ------
#----------------------------------------------------------#

datasubset_all_unique <- purrr::map(datasubset_all,readr::read_rds) %>% bind_rows() %>% 
  distinct(taxa) %>% rename(taxon_name = taxa)

#----------------------------------------------------------#
# 3. Convert taxon names to neotoma names
#----------------------------------------------------------#

datasubset_all_unique_re <- inner_join(datasubset_all_unique,taxa_ref_table, by = "taxon_name") %>% 
  select(neotoma_names) %>% rename(taxon_name = neotoma_names) %>% 
  distinct()

datasubset_all_unique_re_vec <- as.vector.data.frame(datasubset_all_unique_re)

datasubset_all_unique_re_vec_chr <- unlist(datasubset_all_unique_re_vec) %>% 
  as.character() 

#----------------------------------------------------------#
# 4. Get classification per taxon list 
#----------------------------------------------------------#

safe_classification <- purrr::safely(get_classification)   # capture errors 
class_taxa <- purrr::map(datasubset_all_unique_re_vec_chr, ~ safe_classification
                         (taxa_vec = .x, use_only_exact_match = FALSE), 
                         .progress = TRUE) 

class_taxa_tib <- tibble::tibble(class_taxa)  # classification as tibble
class_taxa_tib %>% unnest(class_taxa)
View(class_taxa_tib)

write_rds(class_taxa, here("Data/Processed/Data_harmonised/classified_taxa_neotoma_p1_all_subset.rds"))


#STEP 2: CLASSIFY FAILED TAXA


#----------------------------------------------------------#
# 1. Load data classified taxa rds file  ------------------
#----------------------------------------------------------#

class_taxa <- read_rds(here("Data/Processed/Data_harmonised/classified_taxa_neotoma_p1_all_subset.rds"))

class_taxa_tib <- tibble::tibble(class_taxa)  # classification as tibble
class_taxa_tib %>% unnest(class_taxa)
View(class_taxa_tib)

#----------------------------------------------------------#
# 2. Identify taxa with successful or failed classifications
#----------------------------------------------------------#

class_taxa_tib_has_no_result <- function(x) {   # function to id taxon with failed classifications
  is.null(x$result)
}

class_taxa_checked <-                           # produce checklist of successful or failed classifications: TRUE -> failed
  class_taxa_tib %>% 
  dplyr::mutate(
    has_no_results = purrr::map_lgl(
      .x = class_taxa,.f =
        class_taxa_tib_has_no_result
    )
  ) 

class_taxa_checked %>%  print(n = 500)


class_taxa_has_no_res <- class_taxa_checked %>% select(has_no_results)  # select on classification results column as lgl
View(class_taxa_has_no_res)


class_taxa_has_no_res_chr <- class_taxa_has_no_res %>% as_vector() %>% as.character() %>% as_tibble() # as chr
typeof(class_taxa_has_no_res_chr)


class_taxa_names <- bind_cols(datasubset_all_unique_re,class_taxa_has_no_res_chr) %>% rename(class_res = value)  # bind neotoma_name with classification results 

class_taxa_failed <- class_taxa_names %>% filter(class_res == TRUE)   # list of failed classifications
class_taxa_success <- class_taxa_names %>% filter(class_res == FALSE)   # list of failed classifications

#----------------------------------------------------------#
# 3. Filter out if the classification is plant or an animal 
#----------------------------------------------------------#  

class_taxa_succeeded <- class_taxa_checked %>% # successful classification with tabulate results
  filter(has_no_results == FALSE) %>% 
  unnest(class_taxa) %>% 
  unnest_longer(class_taxa) %>% 
  unpack(cols = class_taxa) %>% 
  unnest_longer(class_taxa$classification) %>% 
  filter(!is.na(id))


class_taxa_animals <- class_taxa_succeeded %>%  # with classification Animalia 
  select(sel_name,classification) %>% 
  unnest_wider(classification) %>% 
  select(sel_name, name, rank) %>% 
  unnest(c(name,rank)) %>% 
  filter(rank == "kingdom") %>% 
  filter(name == "Animalia")

class_taxa_plants <- class_taxa_succeeded %>%  # with classification Plantae 
  select(sel_name,classification) %>% 
  unnest_wider(classification) %>% 
  select(sel_name, name, rank) %>% 
  unnest(c(name,rank)) %>% 
  filter(rank == "kingdom") %>% 
  filter(name == "Plantae")


write_rds(class_taxa_succeeded, here("Data/Processed/Data_harmonised/class_taxa_succeeded_2.rds"))
write_csv(class_taxa_plants, here("Data/Processed/Data_harmonised/class_taxa_plants.csv"))

#----------------------------------------------------------#
# 4. Manual classification ; 0 failed ; 108 classified as animals initially)
#----------------------------------------------------------#  

animals <- class_taxa_animals %>% rename(taxon_name = sel_name)

class_taxa_failed_animal <- full_join(class_taxa_failed, animals, by = "taxon_name") %>% select(taxon_name)

write_csv(class_taxa_failed_animal, here("Data/Processed/Data_harmonised/class_taxa_failed_animal_2.csv"))

class_taxa_failed_animal_re <- read_csv(here("Data/Processed/Data_harmonised/class_taxa_failed_animal_re.csv"))

class_taxa_failed_animal_2 <- read_csv(here("Data/Processed/Data_harmonised/class_taxa_failed_animal_2.csv"))

class_taxa_failed_animal_classified_1 <- inner_join(class_taxa_failed_animal_2, class_taxa_failed_animal_re, by = "taxon_name" ) # already classified
class_taxa_failed_animal_to_classify <- anti_join(class_taxa_failed_animal, class_taxa_failed_animal_re, by = "taxon_name") # not classified

class_taxa_failed_animal_to_classify_1 <- anti_join(class_taxa_failed_animal_to_classify,class_taxa_failed_animal_classified_2, by = "taxon_name") # not classified

write_csv(class_taxa_failed_animal_to_classify_1, here("Data/Processed/Data_harmonised/class_taxa_failed_animal_to_classify_1.csv"))

class_taxa_failed_animal_classified_2 <- read_csv(here("Data/Processed/Data_harmonised/class_taxa_failed_animal_to_classify_1.csv")) # class_taxa_failed_animal_to_classify 'already classified'
class_taxa_failed_animal_classfied_all <- bind_rows(class_taxa_failed_animal_classified_1,class_taxa_failed_animal_classified_2)
class_taxa_failed_animal_re_names <- names((class_taxa_failed_animal_classfied_all))
class_taxa_failed_animal_re_names_new <- class_taxa_failed_animal_classfied_all %>%  rename(level_= class_taxa_failed_animal_re_names, -1) 
new_name <- paste0("level_", 1:8)
class_taxa_re <- class_taxa_failed_animal_re_names_new %>% set_names("taxon_name", new_name)

write_csv(class_taxa_re, here("Data/Processed/Data_harmonised/class_taxa_reclass.csv")) # classified as animals before then reclassified as animals


#STEP 3: CREATE CUSTOM HARMONISATION TABLE


class_taxa_re <- read_csv(here("Data/Processed/Data_harmonised/class_taxa_reclass.csv"))
class_taxa_success   <- read_rds(here("Data/Processed/Data_harmonised/class_taxa_succeeded.rds"))
class_taxa_plants    <- read_csv(here("Data/Processed/Data_harmonised/class_taxa_plants.csv"), show_col_types = FALSE)
taxa_ref_table       <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

#----------------------------------------------------------#
# 1. Create general harmonization table 
#----------------------------------------------------------#

class_taxa_hlist <- class_taxa_success %>% 
  select(sel_name,classification) %>% 
  inner_join(class_taxa_plants, by =  "sel_name") %>%            
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

harmonization_table_gen_combined <- bind_rows(harmonization_table_gen, class_taxa_re) #

write_csv(harmonization_table_gen_combined, here("Data/Processed/Data_harmonised/harmonization_table_new.csv"))

