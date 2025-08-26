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
library(dplyr)
library(mgcv)
library(broom)
#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness <- read_rds(here("Outputs/Data/paper_1_study_3/richness_data_study_3.rds"))


#----------------------------------------------------------#
# 2. Fit rarefied richness and age to a model --
#----------------------------------------------------------# 

#3.1. Fit a GLM model:

model_fam <- glm(richness ~ age, data = richness$family) #family
model_gen <- glm(richness ~ age, data = richness$genus) #genus
model_sp <- glm(richness ~ age, data = richness$species) #species

##### model results - GLM

model_fam_study_1 <- tidy(model_fam) #family
model_gen_study_1 <- tidy(model_gen) #genus
model_sp_study_1 <- tidy(model_sp) #species 

#----------------------------------------------------------#
# 3.2. Fit a GAM model:

###### use gam function

model_fam_gam <- gam(richness ~ age, data = richness$family, method = 'REML') #family
model_gen_gam <- gam(richness ~ age, data = richness$genus, method = 'REML') #genus
model_sp_gam <- gam(richness ~ age, data = richness$species, method = 'REML') #species

#model results - GAM
summary(model_fam_gam) #family
summary(model_gen_gam) #genus
summary(model_sp_gam) #species

plot(model_fam_gam,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2) #family
plot(model_gen_gam,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2) #genus
plot(model_sp_gam,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)  #species

##### use bam function( as in Gordon et al)

bs <-  "cr"; k <- 12
b <- bam(richness ~ s(age, bs=bs, k=k), data = richness$family, method = "GCV.Cp") # method 1
plot(b, pages =1 , rug=FALSE)
plot(b, pages = 1, rug=FALSE, seWithMean = TRUE)
summary(b)

c <- bam(richness ~ s(age, bs=bs, k=k), data =richness$family, method = "REML") # method 2
plot(c, pages =1, rug=FALSE)
plot(c, pages =1, rug = FALSE, seWithMean=TRUE)