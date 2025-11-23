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
library(gratia)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness <- read_csv(here("Data/Paper_1/data_estimate_richness/study2_richness.csv"))

#----------------------------------------------------------#
# 2. Fit rarefied richness and age to a model --
#----------------------------------------------------------# 

richness %>%                                   # some exploratory data analysis
  ggplot(aes(x = richness, breaks = 20)) +
  geom_histogram()

ggplot(richness, aes(x=age, y=richness)) +
  geom_line(size = 0.3) +
  geom_smooth(method = "loess")

# 2.1. Fit the model - dataset_id as random factor, age(time) as smoothing term

richness_fct <- richness %>%                 # convert dataset_id as factor variable
  mutate(dataset_id = as_factor(dataset_id))


model1 <- gam(richness ~ s(age) + s(dataset_id, bs="re"), data = richness_fct)  # gaussian (normal dist. of residuals)

model2 <- gam(richness ~ s(age) + s(dataset_id, bs="re"), data = richness_fct, family = poisson(link = "log"))

model3 <- bam(richness ~ s(age) + s(dataset_id, bs="re"), data = richness_fct, family = poisson())


# 2.2. model summary

summary(model1)
summary(model2)
summary(model3)

## run some basic model checks, including checking
## smoothing basis dimensions.

mgcv::gam.check(model1,pch=19,cex=.8)
mgcv::gam.check(model2,pch=19,cex=.8)
mgcv::gam.check(model3,pch=19,cex=.8)

# plot model diagnostics 

gratia::appraise(model1)  
gratia::appraise(model2)
gratia::appraise(model3)

## show partial residuals

plot(model1,pages=1,residuals=TRUE) 
plot(model2,pages=1,residuals=TRUE) 
plot(model3,pages=1,residuals=TRUE) 

## `with intercept' CI
plot(model1,pages=1,seWithMean=TRUE) 
plot(model2,pages=1,seWithMean=TRUE) 
plot(model3,pages=1,seWithMean=TRUE) 

#----------------------------------------------------------#
# 3. Save model as RDS files --
#----------------------------------------------------------# 

write_rds(model1,here("Data/Paper_1/data_model/study2_model1.rds"))
write_rds(model2,here("Data/Paper_1/data_model/study2_model2.rds"))
write_rds(model3,here("Data/Paper_1/data_model/study2_model3.rds"))
