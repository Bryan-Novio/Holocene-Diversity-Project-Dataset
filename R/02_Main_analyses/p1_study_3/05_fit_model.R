#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 3: Gordon et al
#
#                       
#                          2024
#
# North America, site-based richness (dataset_id,age, 
# 500 bins - rarefy 300 
#
#                 ---- MODEL FITTING ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(mgcv)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness_eu <- read_rds(here("Outputs/Data/paper_1_study_3/richness_data_study_3_eu.rds"))
richness_na <- read_rds(here("Outputs/Data/paper_1_study_3/richness_data_study_3_na.rds"))


richness_eu_fct <- richness_eu %>%                 # convert dataset_id as factor variable
  mutate(dataset_id = as_factor(dataset_id))

richness_na_fct <- richness_na %>%                 
  mutate(dataset_id = as_factor(dataset_id))


richness_eu_12k <- read_rds(here("Outputs/Data/paper_1_study_3/richness_data_study_3_eu_12k.rds"))
richness_na_12k <- read_rds(here("Outputs/Data/paper_1_study_3/richness_data_study_3_na_12k.rds"))
                                                    
richness_eu_12k_fct <- richness_eu_12k %>%                 # convert dataset_id as factor variable   
  mutate(dataset_id = as_factor(dataset_id))                                                 

richness_na_12k_fct <- richness_na_12k %>%                 
  mutate(dataset_id = as_factor(dataset_id)) 

#----------------------------------------------------------#
# 2. Fit rarefied richness and age to BAM model --
#----------------------------------------------------------# 

model_eu <- bam(richness ~ s(age, bs = 'fs', k=10) +
               s(dataset_id, bs = 're'), method = 'fREML', discrete = TRUE, family = gaussian(),
             data = richness_eu_fct)

model_na <- bam(richness ~ s(age, bs = 'fs', k=10) +
              s(dataset_id, bs = 're'), method = 'fREML', discrete = TRUE, family = gaussian(),
             data = richness_na_fct)


model_eu_12k <- bam(richness ~ s(age, bs = 'fs', k=10) +
                  s(dataset_id, bs = 're'), method = 'fREML', discrete = TRUE, family = gaussian(),
                data = richness_eu_12k_fct)

model_na_12k <- bam(richness ~ s(age, bs = 'fs', k=10) +
                  s(dataset_id, bs = 're'), method = 'fREML', discrete = TRUE, family = gaussian(),
                data = richness_na_12k_fct)


summary(model_eu)
summary(model_na)

summary(model_eu_12k)
summary(model_na_12k)


plot.gam(model_eu)
plot.gam(model_na)

plot.gam(model_eu_12k)
plot.gam(model_na_12k)


#----------------------------------------------------------#
# 3. Save model as RDS files --
#----------------------------------------------------------# 

model_eu_s3 <- write_rds(model_eu,here("Outputs/Data/paper_1_study_3/model_eu_s3.rds"))
model_na_s3 <- write_rds(model_na,here("Outputs/Data/paper_1_study_2/model_na_s3.rds"))

model_eu_12k_s3 <- write_rds(model_eu_12k,here("Outputs/Data/paper_1_study_3/model_eu_12k_s3.rds"))
model_na_12k_s3 <- write_rds(model_na_12k,here("Outputs/Data/paper_1_study_3/model_na_12k_s3.rds"))


