
#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
#     
#                          2019
#
# 
#               ---- TREND VISUALIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness <- read_rds(here("Outputs/Data/paper_1_study_1/richness_data_study_1.rds"))

#----------------------------------------------------------#
# 2. Visualize trends --
#----------------------------------------------------------# 

# 2.1. GLM model:

richness$family %>%                           #family
  mutate(age = as.numeric(age)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "glm", se = TRUE, linewidth = 0.5,) +
  theme_classic()

richness$genus %>%                           #genus
  mutate(age = as.numeric(age)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "glm", se = TRUE, linewidth = 0.5,) +
  theme_classic()

richness$species%>%                          #species
  mutate(age = as.numeric(age)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "glm", se = TRUE, linewidth = 0.5,) +
  theme_classic()

#----------------------------------------------------------#
# 2.2. GAM model:

richness$family %>%                     #family
  mutate(age = as.numeric(age)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

richness$genus %>%                      #genus
  mutate(age = as.numeric(age)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

richness$species%>%                     #species
  mutate(age = as.numeric(age)) %>% 
  ggplot(aes(y = richness, x = age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

