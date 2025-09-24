  #----------------------------------------------------------#
  #               Holocene Diversity Project
  #
  #            Paper01| Method 2: Simova et al
  #
  #                       
  #                          2023
  # North America, site-based richness (dataset_id,age, 
  # 1000 bins - rarefy 400 
  #
  #                 ----  BINNING  ----
  #----------------------------------------------------------#
  
  library(tidyverse)
  library(here)
  
  #----------------------------------------------------------#
  # 1. Load data set -----------------------------------------
  #----------------------------------------------------------# 
  
  harmonized_data <- read_rds(here("Outputs/Data/paper_1_study_2/harmonized_data_study_2.rds"))
  woody_taxa <- read_csv(here("Data/Processed/Other/woody_taxa_res.csv"), show_col_types = FALSE)
  
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
  
  source_files <-  sapply(
    paste0("R/Functions/", fun_list, sep = ""),
    source
  )
  
  #----------------------------------------------------------#
  # 3. Filter woody taxa from harmonized dataset ------------
  #----------------------------------------------------------#
  
  harmonized_data_woody <- inner_join(woody_taxa, harmonized_data$genus, by = "taxa", relationship = "many-to-many")
 
  #----------------------------------------------------------#
  # 4. Bin data  at different taxo rank --
  #----------------------------------------------------------# 
  
  # Bin  data 
  
  binned_data <- harmonized_data_woody %>%  bin_data(1000)
  
  # Prepare data for richness estimation
  
  prepared_data_for_richness_estimation <- binned_data %>% 
    prepare_data_for_richness_estimation("binned") %>%
    mutate(sample_id = paste0(dataset_id, "-", age))
  
  #----------------------------------------------------------#
  # 5. Write the binned and prepared_data to RDS files
  #----------------------------------------------------------# 
  
  write_rds(binned_data, here("Outputs/Data/paper_1_study_2/binned_data_study_2.rds"))
  write_rds(prepared_data_for_richness_estimation, here("Outputs/Data/paper_1_study_2/prepared_data_for_richness_estimation_study_2.rds"))
  