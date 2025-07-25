library(tidyverse)
library(here)
library(purrr)
library(dplyr)
library(taxospace)

#----------------------------------------------------------#
# 1. Load data files per dataset (study)------------------
#----------------------------------------------------------#

method_data_files <- list.files("Data/Processed/Other/", pattern ="[.]rds$", full.names = TRUE)
taxa_ref_table    <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

#----------------------------------------------------------#
# 2. Select unique taxa from all dataset from each study ------
#----------------------------------------------------------#

method_data_taxa <- purrr::map(method_data_files,readr::read_rds) %>% bind_rows() %>% 
  distinct(taxa) %>% rename(taxon_name = taxa)

#----------------------------------------------------------#
# 3. Convert taxon names to neotoma names
#----------------------------------------------------------#

method_data_join_neotoma <- inner_join(method_data_taxa,taxa_ref_table, by ='taxon_name')

method_data_join_neotoma_re <- method_data_join_neotoma %>% select(neotoma_names) %>% rename(taxon_name = neotoma_names)

method_data_vec <- as.vector.data.frame(method_data_join_neotoma_re)

method_data_vec_chr <- unlist(method_data_vec) %>% as.character()
str(method_data_vec_chr)

#----------------------------------------------------------#
# 4. Get classification per taxon list 
#----------------------------------------------------------#

safe_classification <- purrr::safely(get_classification)   # capture errors 
class_taxa <- purrr::map(method_data_vec_chr, ~ safe_classification
                         (taxa_vec = .x, use_only_exact_match = FALSE), 
                           .progress = TRUE) 

class_taxa_tib <- tibble::tibble(class_taxa)  # classification as tibble
class_taxa_tib %>% unnest(class_taxa)
View(class_taxa_tib)

class_taxa <- read_rds(here("Data/Processed/Data_harmonised/classified_taxa_neotoma_paper_1.rds"))
write_rds(class_taxa, here("Data/Processed/Data_harmonised/classified_taxa_neotoma_paper_1.rds"))

