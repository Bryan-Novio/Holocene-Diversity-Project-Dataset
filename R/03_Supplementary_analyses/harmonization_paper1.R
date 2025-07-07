#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                Paper01| Harmonization
#
#     
#                          2025
#
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(taxospace)

#----------------------------------------------------------#
# 1. Load data files per dataset (study)------------------
#----------------------------------------------------------#

method_data_files <- list.files("Data/Processed/Other/", pattern ="[.]rds$", full.names = TRUE)
method_data_files

method_data <- purrr::map(method_data_files,readr::read_rds) %>%  bind_rows() %>%  distinct(taxa)
method_data

#----------------------------------------------------------#
# 2. Select taxa column for each dataset -----------------
#----------------------------------------------------------#

method_data_taxa <- purrr::map(method_data, ~ pull(.x, taxa))
method_data_taxa
str(method_data_taxa)
#----------------------------------------------------------#
# 3. Select only unique taxa per taxon list  -------------
#----------------------------------------------------------#

method_data_taxa_unique <- purrr::map(method_data_taxa, ~ unique(.x))
str(method_data_taxa_unique[[1]])
str(method_data_taxa_unique[[2]])
str(method_data_taxa_unique[[3]])
str(method_data_taxa_unique[[4]])
nrow(method_data_taxa_unique[[1]])

typeof(method_data_taxa_unique[[1]])
nrow(method_data_taxa_unique[[1]])

#----------------------------------------------------------#
# 4. Fix format of taxon names (i.e. capitalize first letter) 
#----------------------------------------------------------#

method_data_1_taxa_unique_cap <- str_to_sentence(method_data_taxa_unique[[1]])
str(method_data_1_taxa_unique_cap)  # a subset(from one method)
   
method_data_taxa_unique_cap <- purrr::map(method_data, ~ str_to_sentence(.x))
str(method_data_taxa_unique_cap)        # all subsets (from all methods)
method_data_taxa_unique_cap_1 <- str_replace_all(method_data_taxa_unique_cap,"_"," ")

#----------------------------------------------------------#
# 5. Get classification per taxon list
#----------------------------------------------------------#

#----------------------------------------------------------#
# 5.1. Option A: Get classification per taxon list per study
#----------------------------------------------------------#

method_data_classification_1 <- 
  purrr::map(.x = method_data_taxa_unique_cap_1,
             .f = ~ get_classification(taxa_vec = .x,
              use_only_exact_match = FALSE),.progress = TRUE)  #Giesecke et al

method_data_classification_1

method_data_classification_1[[1]]
method_data_classification_1[[1]]$classification  


method_data_classification_1[[991]]
method_data_classification_1[[991]]$classification # plant or animal?

method_data_classification_2 <- 
  purrr::map(.x = method_data_taxa_unique_cap[[2]],
             .f = ~ get_classification(taxa_vec = .x,
              use_only_exact_match = FALSE),.progress = TRUE) # Simova et al

method_data_classification_2
method_data_classification_2[[1]]
method_data_classification_2[[1]]$classification

method_data_classification_3 <- 
  purrr::map(.x = method_data_taxa_unique_cap[[3]],
             .f = ~ get_classification(taxa_vec = .x,
              use_only_exact_match = FALSE),.progress = TRUE) # Gordon et al

method_data_classification_3
method_data_classification_3[[999]]
method_data_classification_3[[999]]$classification


method_data_classification_4 <- 
  purrr::map(.x = method_data_taxa_unique_cap[[4]],
             .f = ~ get_classification(taxa_vec = .x,
              use_only_exact_match = FALSE),.progress = TRUE) # Bhatta et al

method_data_classification_4
method_data_classification_4[[350]]
method_data_classification_4[[350]]$classification

#----------------------------------------------------------#
# 5.2. Option B: Get classification per taxon list all the same time 
#----------------------------------------------------------#


method_data_classification <- vector("list") # set output format

seq_along(method_data_taxa_unique_cap)      # index per subset
   
for (i in seq_along(method_data_taxa_unique_cap)) {
  method_data_classification[[i]] <- purrr::map(.x = method_data_taxa_unique_cap[[i]],
                           .f = ~ get_classification(taxa_vec = .x,
                                      use_only_exact_match = FALSE),.progress = TRUE)   # generate classification by 'for loop'
}


method_data_classification[[1]] #Giesecke et al  # classify taxa per study
method_data_classification[[2]] # Simova et al
method_data_classification[[3]] # Gordon et al
method_data_classification[[4]] # Bhatta et al


m1 <- method_data_classification[[1]]            # view classification table as a whole
View(m1)
m1_tib <- tibble(m1)
m1_tib %>% unnest_longer(m1) 

m2 <- method_data_classification[[1]][[1]]       # view classification table for one taxa within a subset
m2
m2$classification


