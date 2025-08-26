
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
#               ---- TREND VISUALIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(ggpubr)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness <- read_rds(here("Outputs/Data/paper_1_study_1/richness_data_study_1.rds"))

#----------------------------------------------------------#
# 2. Visualize trends --
#----------------------------------------------------------# 

# 2.1. GLM model:

model_gen_glm <- glm(richness ~ age, data = richness$species)
model_gen_glm <- glm(richness ~ age, data = richness$genus)
model_gen_glm <- glm(richness ~ age, data = richness$species)

###### family


## 2.1.1. Data frame for predictions

new_data <- with(richness$family, data.frame(age = seq(min(age), max(age), length.out = 200)))

# 2.1.2. Predict the values and standard errors on the response scale

predictions <- predict(model_gen_glm, new_data, se.fit = TRUE, type = "response")

# 2.1.3. Add predictions and confidence intervals to the new data frame

new_data$fit <- predictions$fit
new_data$lwr <- predictions$fit - 1.96 * predictions$se.fit
new_data$upr <- predictions$fit + 1.96 * predictions$se.fit

# 2.1.4. Plot the results using ggplot2

ggplot(new_data, aes(x = age, y = fit)) +
  geom_point(data = richness$family, aes(y = richness), alpha = 0.5) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    x = "Age",
    y = "Predicted Richness"
  )


###### genus

## 2.1.1. Data frame for predictions

new_data <- with(richness$genus, data.frame(age = seq(min(age), max(age), length.out = 200)))

# 2.1.2. Predict the values and standard errors on the response scale

predictions <- predict(model_gen_glm, new_data, se.fit = TRUE, type = "response")

# 2.1.3. Add predictions and confidence intervals to the new data frame

new_data$fit <- predictions$fit
new_data$lwr <- predictions$fit - 1.96 * predictions$se.fit
new_data$upr <- predictions$fit + 1.96 * predictions$se.fit

# 2.1.4. Plot the results using ggplot2

ggplot(new_data, aes(x = age, y = fit)) +
  geom_point(data = richness$genus, aes(y = richness), alpha = 0.5) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    x = "Age",
    y = "Predicted Richness"
  )

###### species


## 2.1.1. Data frame for predictions

new_data <- with(richness$species, data.frame(age = seq(min(age), max(age), length.out = 200)))

# 2.1.2. Predict the values and standard errors on the response scale

predictions <- predict(model_gen_glm, new_data, se.fit = TRUE, type = "response")

# 2.1.3. Add predictions and confidence intervals to the new data frame

new_data$fit <- predictions$fit
new_data$lwr <- predictions$fit - 1.96 * predictions$se.fit
new_data$upr <- predictions$fit + 1.96 * predictions$se.fit

# 2.1.4. Plot the results using ggplot2

ggplot(new_data, aes(x = age, y = fit)) +
  geom_point(data = richness$species, aes(y = richness), alpha = 0.5) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    x = "Age",
    y = "Predicted Richness"
  )


# 2.2. GAM model:





