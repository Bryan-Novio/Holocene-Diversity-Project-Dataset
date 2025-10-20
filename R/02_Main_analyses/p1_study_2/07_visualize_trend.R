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
library(ggplotify)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness <- read_rds(here("Outputs/Data/paper_1_study_2/richness_data_study_2.rds"))
richness_re <- read_rds(here("Outputs/Data/paper_1_study_2/richness_data_study_2_re.rds"))
model_1_s2 <- read_rds(here("Outputs/Data/paper_1_study_2/model_1_s2.rds"))
model_2_s2 <- read_rds(here("Outputs/Data/paper_1_study_2/model_2_s2.rds"))

#----------------------------------------------------------#
# 2. Visualize trends 
#----------------------------------------------------------# 

#2.1. using actual data 

## without RE

richness %>%
  ggplot(aes(x = age, y = richness)) +
  geom_smooth(aes(group=1), method = "gam", formula = y ~ s(x) ) +
  scale_x_reverse() +
  geom_vline(xintercept = 9800, color = 'red') +
  theme_classic()

## with RE (geom_smooth does not allow RE in gam model from mgcv) (not working)

dataset_id <-  factor(richness$dataset_id)

ggplot(richness, aes(x = age, y = richness)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x) + s(dataset_id, bs="re")) +
  scale_x_reverse() +
  geom_vline(xintercept = 9800, color = 'red') +
  theme_classic()

## use plot_smooth from 'itsadug' package (use base R for plotting)

itsadug::plot_smooth(model_1, view = 'age', rm.ranef = TRUE, rug = FALSE, eegAxis = FALSE , xlab = "age", ylab = "richness")
itsadug::plot_smooth(model_2_s2, view = 'age', rm.ranef = TRUE, rug = FALSE, eegAxis = FALSE , xlab = "age", ylab = "richness")

##reverse x-axis 


p <- ggplotify::as.ggplot(
  function() {
    # Plot the smooth term
    p <- plot_smooth(model_1_s2, view = 'age', rm.ranef = TRUE, rug = FALSE)
    
    # Add the scale_x_reverse() layer to the ggplot object 'p'
    p  + scale_x_reverse() 
  }
)

p 

#2.2. using predicted data (-- fit the GAM with mgcv and plot its predictions)


# model_1_s2

newdata <- data.frame(age = seq(from = min(richness$age), to = max(richness$age), length.out = 100),
                      dataset_id = factor(1001))

predictions <- predict(model_1_s2, newdata = newdata, se.fit = TRUE, exclude = "s(dataset_id)")

newdata$fit <- predictions$fit 
newdata$se <- predictions$se.fit


ggplot(richness, aes(x = age, y = richness)) +
  geom_point(alpha = 0.5) +
  geom_line(data = newdata, aes(y = fit), color = "black", linewidth = 1) +
  geom_ribbon(data = newdata, aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se, y = NULL), fill = "grey", alpha = 0.4) +
  geom_vline(xintercept = 9800, color = "red") +
  theme_classic() +
  scale_x_reverse()

ggplot(richness, aes(x = age, y = richness)) +
  geom_line(data = newdata, aes(y = fit), color = "black", linewidth = 1) +
  geom_ribbon(data = newdata, aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se, y = NULL), fill = "grey", alpha = 0.4) +
  geom_vline(xintercept = 9800, color = "red") +
  theme_classic() +
  scale_x_reverse()

# model_2_s2

newdata <- data.frame(age = seq(from = min(richness$age), to = max(richness$age), length.out = 100),
                      dataset_id = factor(1001))

predictions <- predict(model_2_s2, newdata = newdata, se.fit = TRUE, exclude = "s(dataset_id)")

newdata$fit <- predictions$fit 
newdata$se <- predictions$se.fit


ggplot(richness, aes(x = age, y = richness)) +
  geom_point(alpha = 0.5) +
  geom_line(data = newdata, aes(y = fit), color = "black", linewidth = 1) +
  geom_ribbon(data = newdata, aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se, y = NULL), fill = "grey", alpha = 0.4) +
  geom_vline(xintercept = 9800, color = "red") +
  theme_classic() +
  scale_x_reverse()
 
ggplot(richness, aes(x = age, y = richness)) +
  geom_line(data = newdata, aes(y = fit), color = "black", linewidth = 1) +
  geom_ribbon(data = newdata, aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se, y = NULL), fill = "grey", alpha = 0.4) +
  geom_vline(xintercept = 9800, color = "red") +
  theme_classic() +
  scale_x_reverse()
