library(tidyverse)
library(here)
library(purrr)
library(dplyr)
library(taxospace)


#----------------------------------------------------------#
# 1. Load data files per dataset (study)------------------
#----------------------------------------------------------#

method_data_files <- list.files("Data/Processed/Other/", pattern ="[.]rds$", full.names = TRUE)
method_data_files

#----------------------------------------------------------#
# 2. Select unique from all dataset from each study ------
#----------------------------------------------------------#

method_data <- purrr::map(method_data_files,readr::read_rds) %>% 
  bind_rows() %>%  
  distinct(taxa)

method_data_un <- unlist(method_data)

#----------------------------------------------------------#
# 3. Fix format of taxon names (i.e. capitalize first letter) 
#----------------------------------------------------------#

method_data_vec <- as.vector.data.frame(method_data_un)
method_data_cap <- str_to_sentence(method_data_vec)
method_data_form <- str_replace_all(method_data_cap,"_"," ")
str(method_data_form)
method_data_form_tib <- tibble(method_data_form)
method_data_re <- rename(method_data_form_tib, taxon_name = method_data_form)

#----------------------------------------------------------#
# 4. Load taxa_ref_table
#----------------------------------------------------------#

taxa_ref_table <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"))


#----------------------------------------------------------#
# 5. Translate "taxa" to Neotoma taxa names
#----------------------------------------------------------#

neo_taxa <- taxa_ref_table %>% 
  semi_join(method_data_re, join_by(neotoma_names == taxon_name)) %>% # select neotoma_names that match taxon name from all studies
  select(neotoma_names)

neo_taxa
neo_taxa_re <- rename(neo_taxa, taxon_name = neotoma_names)

str(neo_taxa_re)

neo_taxa_vec<- unlist(neo_taxa_re) %>% as.character()

neo_taxa_vec
str(neo_taxa_vec)

neo_taxa_vec[72]
neo_taxa_vec_1 <- neo_taxa_vec[-c(72:90)]
get_classification(taxa_vec = neo_taxa_vec[72], use_only_exact_match = FALSE)

#----------------------------------------------------------#
# 6. Get classification per taxon list all the same time 
#----------------------------------------------------------#

# with error

method_data_class <- 
  purrr::map(.x = neo_taxa_vec_1,
             .f = ~ get_classification(taxa_vec = .x,
               use_only_exact_match = FALSE),
                .progress = TRUE) 

method_data_class
method_data_class[[30]]$classification

# this one is working

safe_classification <- purrr::safely(get_classification)   # capture errors 
class_taxa <- purrr::map(neo_taxa_vec, ~ safe_classification
                         (taxa_vec = .x, use_only_exact_match = FALSE), 
                           .progress = TRUE) 

class_taxa

class_taxa[[99]]                        # succeeded
class_taxa[[99]]$result
class_taxa[[99]]$result$classification

class_taxa[[72]]                         # failed
class_taxa[[72]]$result
class_taxa[[72]]$result$classification

class_taxa[[617]]                        # failed
class_taxa[[617]]$result
class_taxa[[617]]$result$classification

class_taxa$result$classification

class_taxa[[618]]                        # plant/animal
class_taxa[[618]]$result
class_taxa[[618]]$result$classification


#----------------------------------------------------------#
# 7. Filter out if the classification is plant or an animal 
#----------------------------------------------------------#

# Extract names where classification includes 'Animalia'

class_with_animalia <- safely(function(x) {
  classification_df <- x$result$classification[[1]]
  if (!is.null(classification_df) && "name" %in% colnames(classification_df)) {
    if ("Animalia" %in% classification_df$name) {
      return(x$result$sel_name)
    }
  }
  return(NA_character_)
})

class_with_animalia_res <- map(class_taxa, class_with_animalia)

View(class_with_animalia_res)

class_with_animalia_list <- map(class_with_animalia_res, "result") %>%
  flatten_chr() %>% 
  discard(is.na)

typeof(class_with_animalia_list)

