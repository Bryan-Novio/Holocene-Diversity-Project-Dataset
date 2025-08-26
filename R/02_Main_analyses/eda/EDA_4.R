
library(tidyverse)
library(here)
library(dplyr)
library(mgcv)
library(ggpubr)
library(ggplot2)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

harmonized_data <- read_rds(here("Outputs/Data/paper_1_study_1/harmonized_data_study_1.rds"))


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
# 3. Bin data  at different taxo rank --
#----------------------------------------------------------# 

# bins = 500; 1000, non-binned

binned_data_500 <- purrr::map(harmonized_data, ~ bin_data(.x, 500)) 
binned_data_1000 <- purrr:::map(harmonized_data,~ bin_data(.x, 1000))
non_binned_data <-  harmonized_data

# # Prepare data for richness estimation

prepared_data_for_richness_estimation_500 <- 
  purrr::map(binned_data_500, ~ prepare_data_for_richness_estimation(.x, "binned")) %>%
  purrr::map( ~ dplyr::mutate(.x, sample_id = paste0(dataset_id, "-", age)))

prepared_data_for_richness_estimation_1000 <- 
  purrr::map(binned_data_1000, ~ prepare_data_for_richness_estimation(.x, "binned")) %>%
  purrr::map( ~ dplyr::mutate(.x, sample_id = paste0(dataset_id, "-", age)))

prepared_data_for_richness_estimation_nonbinned <- 
  purrr::map(non_binned_data, ~ prepare_data_for_richness_estimation(.x, "nonbinned")) %>%
  purrr::map( ~ dplyr::mutate(.x, sample_id = paste0(dataset_id, "-", age)))

#----------------------------------------------------------#
# 3. Rarefy data  at different taxo rank --
#----------------------------------------------------------# 

##### s1

rarefied_data_s1 <- purrr::map(prepared_data_for_richness_estimation_1000, ~ rarefy_all_samples_iter(
  data_source =.,n_grains = 500, n_iter = 10)) %>% 
  purrr::map (~ separate_wider_delim(.x,sample_id, "-", names = c("sample_id","age")))

##### s2 

rarefied_data_s2 <- purrr::map(prepared_data_for_richness_estimation_1000, ~ rarefy_all_samples_iter(
  data_source =.,n_grains = 400, n_iter = 10)) %>% 
  purrr::map (~ separate_wider_delim(.x,sample_id, "-", names = c("sample_id","age")))

##### s3

rarefied_data_s3 <- purrr::map(prepared_data_for_richness_estimation_500, ~ rarefy_all_samples_iter(
  data_source =.,n_grains = 300, n_iter = 10)) %>% 
  purrr::map (~ separate_wider_delim(.x,sample_id, "-", names = c("sample_id","age")))

##### s4

rarefied_data_s4 <- purrr::map(prepared_data_for_richness_estimation_nonbinned, ~ rarefy_all_samples_iter(
  data_source =.,n_grains = 300, n_iter = 10)) %>% 
  purrr::map (~ separate_wider_delim(.x,sample_id, "-", names = c("sample_id","age")))


rarefied_data_s1$genus %>% arrange(desc(age))

#----------------------------------------------------------#
# 4. Estimate richness  at different taxo rank --
#----------------------------------------------------------# 

richness_s1 <- purrr::map(rarefied_data_s1, ~ estimate_richness(.x)) %>% 
  purrr::map( ~ dplyr::mutate(.x,age = as.numeric(age)))

richness_s2 <- purrr::map(rarefied_data_s2, ~ estimate_richness(.x)) %>% 
  purrr::map( ~ dplyr::mutate(.x,age = as.numeric(age)))

richness_s3 <- purrr::map(rarefied_data_s3, ~ estimate_richness(.x)) %>% 
  purrr::map( ~ dplyr::mutate(.x,age = as.numeric(age)))

richness_s4 <- purrr::map(rarefied_data_s4, ~ estimate_richness(.x)) %>% 
  purrr::map( ~ dplyr::mutate(.x,age = as.numeric(age)))

richness_s1$genus %>% arrange(desc(age))
#----------------------------------------------------------#
# 5. Fit rarefied richness and age to a model --
#----------------------------------------------------------# 

############ genus level

##### s1

# BAM
bs <-  "cr"; k <- 10

model_1 <- bam(richness ~ s(age, bs=bs, k=k), data = richness_s1$genus, method = "GCV.Cp", iden) 
mod_1 <- plot(model_1, pages = 1, rug=FALSE, seWithMean = TRUE)
summary(model_1)


##### s2

model_2 <- bam(richness ~ s(age, bs=bs, k=k), data = richness_s2$genus, method = "GCV.Cp") 
mod_2 <- plot(model_2, pages = 1, rug=FALSE, seWithMean = TRUE)
summary(model_2)

##### s3

model_3 <- bam(richness ~ s(age, bs=bs, k=k), data = richness_s3$genus, method = "GCV.Cp") 
mod_3 <- plot(model_3, pages = 1, rug=FALSE, seWithMean = TRUE, xlim = c(0, 5000))
summary(model_3)


##### s4

model_4 <- bam(richness ~ s(age, bs=bs, k=k), data = richness_s4$genus, method = "GCV.Cp") 
mod_4 <- plot(model_4, pages = 1, rug=FALSE, seWithMean = TRUE, xlim = c(0, 5000))
summary(model_4)

plot_mod_all <- ggarrange(mod_1, mod_2,mod_3,mod_4, common.legend = TRUE,nrow = 1)











