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

## 1.1. Load data set

data_rich <- list.files(
  "Data/Paper_1/data_estimate_richness/s3_richness",
  pattern = "[.]rds$",
  full.names = TRUE
)

data_back <- list.files(
  "Data/Paper_1/data_model/data_back",
   pattern = "[.]rds$",
   full.names = TRUE)

## 1.2. Load functions

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

#----------------------------------------------------------#
# 2. Visualize trends --
#----------------------------------------------------------# 

##2.1. for single iteration

readr::read_rds(rich_data[[1]]) %>% summary()

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


