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
#               ---- MODEL FITTING ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(mgcv)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness <- read_rds(here("Outputs/Data/paper_1_study_2/richness_data_study_2.rds"))

#----------------------------------------------------------#
# 2. Fit rarefied richness and age to a model --
#----------------------------------------------------------# 

# 2.1. Fit the GAMM model - dataset_id as random factor, age(time) as smoothing term

richness_re <- richness %>%                 # convert dataset_id as factor variable
  mutate(dataset_id = as_factor(dataset_id))

model <- gam(richness ~ s(age) + s(dataset_id, bs="re"), data = richness_re, method = "REML")

# 2.2. model summary

summary(model)
plot.gam(model)

#----------------------------------------------------------#
# 3. Save model and richness_re as RDS files --
#----------------------------------------------------------# 

richness_re <- write_rds(richness_re,here("Outputs/Data/paper_1_study_2/richness_data_study_2_re.rds"))
model_s2 <- write_rds(model,here("Outputs/Data/paper_1_study_2/model_s2.rds"))
