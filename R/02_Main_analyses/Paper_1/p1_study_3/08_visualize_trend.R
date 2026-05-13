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
#            ---- TREND VISUALIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

purrr::map(1:10, 
           .progress = TRUE,
           .f = load_pred_richness_and_select) %>% 
  bind_rows() %>% 
  group_by(region, age) %>% 
  summarise(
    conitental_median_richness = median(richness),
    conitental_richness_upp = quantile(richness, 0.975),
    conitental_richness_dwn = quantile(richness, 0.025)
  )

#----------------------------------------------------------#
# 2. Visualize trends --
#----------------------------------------------------------# 

#2.1. using actual data 

## without RE

richness_eu %>%
  ggplot(aes(x = age, y = richness)) +
  geom_point() +
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

plot(model_eu_s3, select=1)
plot(model_na_s3, select=1)

itsadug::plot_smooth(model_eu_s3, view = 'age', rm.ranef = TRUE, rug = FALSE, eegAxis = FALSE , xlab = "age", ylab = "richness")
itsadug::plot_smooth(model_na_s3, view = 'age', rm.ranef = TRUE, rug = FALSE, eegAxis = FALSE , xlab = "age", ylab = "richness")

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


################## use for data visualization site richness and median richness per iteration

ggplot(data_back, aes(x = age, y = richness)) +
  geom_line(data = readr::read_rds(paths[[1]]), aes(group = dataset_id), linewidth = 0.1, alpha = 0.1) +
  geom_line(linewidth = 1, color= "red") +
  geom_ribbon(aes(ymin = rich_low, ymax = rich_high, y = NULL), fill = "blue", alpha = 0.4) +
  facet_wrap(~ region) +
  theme_classic() +
  scale_x_reverse()




readr::read_rds(paths[[1]]) %>% summary()

x <- readr::read_rds(paths[[1]])

iter_1 <- data_back %>% 
  select(region, age, richness)


iter_2 <- data_back %>% 
  select(region, age, richness)


bind_rows(
  iter_1,
  iter_2
) %>% 
  group_by(region, age) %>% 
  summarise(
    median_richness = median(richness),
    upr = quantile(richness, 0.975),
    lwr = quantile(richness, 0.25)
  )












