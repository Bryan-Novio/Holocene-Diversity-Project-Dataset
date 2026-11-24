#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 4: Bhatta et al
#
#
#                       
#                          2023
# Asia, site-based richness (dataset_id,age)
# nonbinned  
#
#    ----RAREFACTION & RICHNESS ESTIMATION(Hill's N)  ----
#----------------------------------------------------------#


library(tidyverse)
library(here)
library(iNEXT)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_study4_harmonised <- 
  read_rds(here("Data/Paper_1/data_harmonize/data_study4_harmonised.rds"))  %>% 
rename(taxa = "taxon_name",
 pollen_grains = "pollen_counts") 

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

#----------------------------------------------------------#
# 3. Rarefy data  at different taxo rank --
#----------------------------------------------------------# 

set.seed(1234)

data_rarefy_then_hill <- 
  data_study4_harmonised %>% 
  split(.$dataset_id) %>% 
  purrr::map(~ {
    data <- 
    .x %>% 
      tidyr::pivot_wider(
        names_from = taxa,
        values_from = pollen_grains,
        values_fill = 0
      ) %>% 
      mutate(age = as.character(age)) %>% 
      unite(
        "dataset_id_age",
        c("dataset_id", "age"),
        sep = "_"
      ) %>% 
      tibble::column_to_rownames("dataset_id_age") %>% 
      as.matrix() %>% 
      t() %>% 
      iNEXT::estimateD(
        q = 0,
        level = 150,
        nboot = 50
      )
    
    
    data_tib <- 
      data %>%
      as_tibble() %>% 
      tidyr::separate(Assemblage, c("dataset_id","age"), sep = "_") %>% 
      select(dataset_id,age,qD) %>% 
      rename(richness = qD) %>% 
      mutate(age = as.double(age))
  })

##combine as one dataframe

data_rarefy_then_hill_all <- 
  data_rarefy_then_hill %>% 
  list_rbind()


#----------------------------------------------------------#
# 4. Write results as csv file
#----------------------------------------------------------# 

write_csv(data_rarefy_then_hill_all, here("Data/Paper_1/data_rarefy/data_rarefy_then_hill.csv"))

