
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
library(ggplot2)
library(ggpubr)
library(mgcv)

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


# Assuming your model is already run
 model_gam_identity <- gam(richness ~ s(age), data = richness$genus, family = gaussian(link = "identity"))

# Create a data frame for predictions
new_data <- with(richness$genus, data.frame(age = seq(min(age), max(age), length.out = 200)))

# Predict the smooth term
# Use type = "lpmatrix" to get the design matrix for the smooth term
# This allows you to reconstruct the effect with standard errors
lp_matrix <- predict(model_gam_identity, new_data, type = "lpmatrix")
smooth_effect <- lp_matrix %*% coef(model_gam_identity)      
smooth_se <- sqrt(rowSums((lp_matrix %*% vcov(model_gam_identity)) * lp_matrix))

# Construct the data frame for plotting
plot_data <- data.frame(
  age = new_data$age,
  fit = smooth_effect + coef(model_gam_identity)["(Intercept)"],
  lwr = smooth_effect + coef(model_gam_identity)["(Intercept)"] - 1.96 * smooth_se,
  upr = smooth_effect + coef(model_gam_identity)["(Intercept)"] + 1.96 * smooth_se
)

# Plot the results using ggplot2
ggplot(plot_data, aes(x = age, y = fit)) +
  # Add confidence interval ribbon
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  # Add smooth line
  geom_line() +
  # Add raw data points
  geom_point(data = richness$genus, aes(x = age, y = richness)) +
  # Add labels and title
  labs(
    title = "GAM Partial Effects Plot for 'age'",
    y = "Predicted Richness",
    x = "Age"
  ) +
  # Apply a clean theme
  theme_minimal()





