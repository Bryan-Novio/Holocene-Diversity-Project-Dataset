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


model_1 <- gam(richness ~ s(age) + s(dataset_id, bs="re"), data = richness_re)  # gaussian (normal dist. of residuals)
model_2 <- gam(richness ~ s(age) + s(dataset_id, bs="re"), data = richness_re, family = poisson(link = "log"))

# 2.2. model summary

summary(model_1)
summary(model_2)

plot(model, select = 1)

plot.gam(model_1)

#----------------------------------------------------------#
# 3. Save model and richness_re as RDS files --
#----------------------------------------------------------# 

richness_re <- write_rds(richness_re,here("Outputs/Data/paper_1_study_2/richness_data_study_2_re.rds"))
model_1_s2 <- write_rds(model_1,here("Outputs/Data/paper_1_study_2/model_1_s2.rds"))
model_2_s2 <- write_rds(model_2,here("Outputs/Data/paper_1_study_2/model_2_s2.rds"))
