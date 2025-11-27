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
#               ---- TREND VISUALIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(mgcv)
library(itsadug)
library(gratia)
library(tidygam)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data_richness <- read_csv(here("Data/Paper_1/data_estimate_richness/study2_richness.csv"))
model1 <- read_rds(here("Data/Paper_1/data_model/study2_model1.rds"))
model2 <-read_rds(here("Data/Paper_1/data_model/study2_model2.rds"))
model3 <-read_rds(here("Data/Paper_1/data_model/study2_model3.rds"))

#----------------------------------------------------------#
# 2. Visualize trends 
#----------------------------------------------------------# 

#2.1. using actual data 

## use plot_smooth from 'itsadug' package (use base R for plotting)

model1_plot <- itsadug::plot_smooth(
  x = model1,
  view = "age",
  rug = TRUE,
  rm.ranef = FALSE
)

model2_plot <- itsadug::plot_smooth(
  x = model2,
  view = "age",
  rug = TRUE,
  rm.ranef = FALSE
)

model3_plot <- itsadug::plot_smooth(
  x = model3,
  view = "age",
  rug = TRUE,
  rm.ranef = FALSE
  
)

## plot the ‘partial effects’ of each smooth term

gratia::draw(model1)
gratia::draw(model2)
gratia::draw(model3)


#2.2. using predicted data (-- fit the GAM with mgcv and plot its predictions)

#model1

# option1

preds_1 <- tidygam::predict_gam(model1, length_out = 50)
plot(preds_1, "age", "dataset_id")


