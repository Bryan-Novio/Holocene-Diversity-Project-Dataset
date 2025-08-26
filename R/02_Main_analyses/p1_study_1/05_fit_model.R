
#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
# Europe, site-based richness (dataset_id,age), 1000 bins - 
# rarefy 500  
#                          2019
#
# 
#               ---- MODEL FITTING ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(broom)
library(mgcv)


#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness <- read_rds(here("Outputs/Data/paper_1_study_1/richness_data_study_1.rds"))

#----------------------------------------------------------#
# 2. Fit rarefied richness and age to a model --
#----------------------------------------------------------# 

##### 2.1. Fit a GLM model - because data distribution is not normal

model_glm <- purrr::map(richness, ~ stats::glm (data =., richness ~ age))

### 2.1.1. Tabulate GLM model results 

model_glm_results <-  purrr::map(model_glm, ~ tidy(., conf.int = TRUE, conf.level = 0.95))

### 2.1.2.  Plot model and some  diagnostics

plot(model_glm$family)
plot(model_glm$genus)
plot(model_glm$species)

#----------------------------------------------------------#

##### 2.2. Fit a GAM model # gam() fucntion # lower GCV score ~ better fit

model_gam <- purrr::map(richness, ~ mgcv::gam(data =., richness ~ age), method = 'REML')

model_gam_fam <-  gam(richness ~ age,  data = richness$family, method = 'REML')
model_gam_gen <-  gam(richness ~ age,  data = richness$genus, method = 'REML')
model_gam_sp <-  gam(richness ~ age,  data = richness$species, method = 'REML')

### 2.2.1. Tabulate GAM model results 

model_gam_results <-  purrr::map(model_gam, ~ summary(.))

### 2.2.2.  Plot GAM model

plot(model_gam_fam, pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2) 
plot(model_gam_gen,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2) 
plot(model_gam_sp,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)  

#----------------------------------------------------------#

##### 2.3. Fit a BAM model # bam() function ( as in Gordon et al) using cr [cubic regression spline], if k - higher more wiggly

bs <-  "cr"; k <- 12

model_bam <- purrr::map (richness, ~ mgcv::bam(data = ., richness ~ s(age, bs=bs, k=k), method = "REML"))

### 2.3.1. Tabulate BAM model results 

model_bam_results <-  purrr::map(model_bam, ~ summary(.))

### 2.3.2.  Plot BAM model

plot(model_bam$family, pages =1, rug = FALSE, seWithMean=TRUE)
plot(model_bam$genus, pages =1, rug = FALSE, seWithMean=TRUE)
plot(model_bam$species, pages =1, rug = FALSE, seWithMean=TRUE)
