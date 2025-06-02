#----------------------------------------------------------#
#
#
#               Holocene Diversity Project
#
#                       Binning
#
#               B.V. Novio & O. Mottl
#                        2025
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# Functions for compilation and binned data                 
#----------------------------------------------------------#

library(tidyverse)
library(here)

data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))


#----------------------------------------------------------#
# 1.  Function bin_data(), output data with binned pollen grains          
#----------------------------------------------------------#


bin_data <- function(data_compilation, bin_size){
  
  pollen_counts <- data_compilation %>% 
    select(dataset_id, raw_counts) %>% 
    unnest(raw_counts) %>% 
    pivot_longer(
      cols = !c(dataset_id,sample_id),
      names_to = "taxa", values_to = "pollen_counts",
      values_drop_na = TRUE)
  
  data_age <- data_compilation %>% 
    select(dataset_id, levels) %>% 
    unnest(levels) %>% 
    select(dataset_id,sample_id, age)
  
  binned_data <- inner_join(pollen_counts, data_age,
                         by = c("dataset_id", 'sample_id')) %>% 
    mutate(BIN = cut(age, seq(min(age), 
                              max(age) + bin_size, bin_size), right = FALSE))
}



BINS <- bin_data(data, 500)   # bin_size = 500          # create bins 
BINS <- bin_data(data, 1000)  # bin_size = 1000



BINS_rec <- BINS %>% 
  mutate(BIN_chr = as.character(BIN)) %>% 
  mutate(BIN_fct = as.factor(BIN_chr)) %>% 
  mutate(BIN_int = as.numeric(BIN_fct) %>%  # recode BINS to integer
           as.factor()) 

  
BINS_rec_2 <- mutate_if(BINS_rec, is.factor, 
                           ~ as.numeric(as.character(.x)))  # Convert BIN from fct to dbl 


binned_data <- BINS_rec_2 %>% 
  select(!BIN)%>% select(!BIN_fct) %>% rename(BIN = BIN_int) # Exclude unnecessary cols


binned_data %>%   #count bins
  count(BIN)

#----------------------------------------------------------#
# 2. Function prepare_data_for_richness_estimation(), unified format        
#----------------------------------------------------------#

prepare_data_for_richness_estimation <- function(data_compilation, data_bin){
  pollen_counts <- data_compilation %>% 
    select(dataset_id, raw_counts) %>% 
    unnest(raw_counts) %>% 
    pivot_longer(
      cols = !c(dataset_id,sample_id),
      names_to = "taxa", values_to = "pollen_counts",
      values_drop_na = TRUE) 
  
  summed_pollen_counts <- pollen_counts %>% 
    group_by(taxa, dataset_id) %>% 
    summarise(summed_pollen_counts = sum(pollen_counts))
  
  data_age <- data_compilation %>% 
    select(dataset_id, levels) %>% 
    unnest(levels) %>% 
    select(dataset_id, age)
  
  summed_pollen_counts_age <- inner_join(summed_pollen_counts, data_age,
             by = "dataset_id")
  
  prepared_data_for_richness_estimation <- 
    inner_join(summed_pollen_counts_age,data_compilation, by = "dataset_id")
  
  prepared_data_for_richness_estimation_2 <- 
    inner_join(data_bin,data_compilation, by = "dataset_id")
}


prepared_data_for_richness_estimation <- prepare_data_for_richness_estimation(data, binned_data)

#----------------------------------------------------------#
# 3. Function estimate_richness(), age(real or bin), richness     
#----------------------------------------------------------#


estimate_richness <- function(data_for_richness_estimation){
  data_for_richness_estimation %>% 
    mutate(present = ifelse(pollen_counts >= 1, 1, 0)) %>% 
      group_by(age, BIN ) %>% 
        summarize(richness = sum(present, na.rm = TRUE, .groups = NULL))
  }

richness_age_bin <-  estimate_richness(prepared_data_for_richness_estimation)

View(richness_age_bin)


