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

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness_re <- read_rds(here("Outputs/Data/paper_1_study_2/richness_data_study_2_re.rds"))
model_s2 <- read_rds(here("Outputs/Data/paper_1_study_2/model_s2.rds"))

#----------------------------------------------------------#
# 2. Visualize trends --
#----------------------------------------------------------# 

newdata <- data.frame(age = seq(from = min(richness_re$age), to = max(richness_re$age), length.out = 100),
                      dataset_id = factor(1001))

predictions <- predict(model_s2, newdata = newdata, se.fit = TRUE, exclude = "s(dataset_id)")

newdata$fit <- predictions$fit
newdata$se <- predictions$se.fit


ggplot(richness_re, aes(x = age, y = richness)) +
  geom_point(alpha = 0.5) +
  geom_line(data = newdata, aes(y = fit), color = "black", linewidth = 1) +
  geom_ribbon(data = newdata, aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se, y = NULL), fill = "grey", alpha = 0.4) +
  geom_vline(xintercept = 9800, color = "red") +
  theme_classic() +
  scale_x_reverse()
