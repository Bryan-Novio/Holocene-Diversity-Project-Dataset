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
# 1. Set -up -----------------------------------------
#----------------------------------------------------------# 
## 1.1. Load functions

# Load the function into the global environment

fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "\\.R$",
    recursive = TRUE
  )

source_files <-
  sapply(
    paste0("R/Functions/", fun_list, sep = ""),
    source
  )

## 1.2. Load data set

median_richness_back <- 
  purrr::map(1:1000, 
           .progress = TRUE,
           .f = load_pred_richness_and_select) %>% 
  bind_rows() %>% 
  group_by(region, age) %>% 
  summarise(
    continental_median_richness = median(richness),
    continental_richness_upp = quantile(richness, 0.975),
    continental_richness_dwn = quantile(richness, 0.025)
  )

data_rich <- list.files(
  "Data/Paper_1/data_estimate_richness/s3_richness",
  pattern = "[.]rds$",
  full.names = TRUE
)

data_back <- list.files(
  "Data/Paper_1/data_model/data_back",
  pattern = "[.]rds$",
  full.names = TRUE)


#----------------------------------------------------------#
# 2. Visualise continental trends -------------------------
#----------------------------------------------------------# 

# All continents in one plot

median_richness_back %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
  geom_line(linewidth = 1, color = "red") + 
  geom_ribbon(aes(ymin = continental_richness_dwn, 
              ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  facet_wrap(~region) +
  theme_classic()+
  scale_x_reverse()


#Europe

median_richness_back %>% 
  filter(region == "Europe") %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
  geom_line(linewidth = 1, color = "red") + 
  geom_ribbon(aes(ymin = continental_richness_dwn, 
                  ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  labs(x = "Age(cal yr BP)" , y = "Richness") +
  theme_classic()+
  scale_x_reverse()

# Asia

median_richness_back %>% 
  filter(region == "Asia") %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
  geom_line(linewidth = 1, color = "red") + 
  geom_ribbon(aes(ymin = continental_richness_dwn, 
                  ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  labs(x = "Age(cal yr BP)" , y = "Richness") +
  theme_classic()+
  scale_x_reverse()

# N. America


median_richness_back %>% 
  filter(region == "North America") %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
  geom_line(linewidth = 1, color = "red") + 
  geom_ribbon(aes(ymin = continental_richness_dwn, 
                  ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  labs(x = "Age(cal yr BP)" , y = "Richness") +
  theme_classic()+
  scale_x_reverse()


#----------------------------------------------------------#
# 3. Visualize trends per iteration with site-level trends --
#----------------------------------------------------------# 

##2.1. for single iteration

readr::read_rds(data_rich[[1]]) %>% summary()

### e.g. for iteration 1

plot_richness_iter(data_back,data_rich,1)

##2.2. for all iterations and summarized to one trend line

all_data_back_iters <- purrr::map(
  .progress = TRUE,
  .x  = data_back,
  .f  =  ~ {
    iters <- .x %>% 
      readr::read_rds()
    
    all_iters <- iters %>% 
      bind_rows() %>% 
      group_by(region, age) %>% 
      summarise(
        median_richness = median(richness),
        upr = quantile(richness, 0.975),
        lwr = quantile(richness, 0.25)
      )
    
  }
)

plot_richness_iter(all_data_back_iters,data_rich,1)

