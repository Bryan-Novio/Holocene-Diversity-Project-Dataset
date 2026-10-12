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

#----------------------------------------------------------#
# 2. Load datasets -----------------------------------------
#----------------------------------------------------------# 

data_rich <- list.files(
  "Data/Paper_1/data_estimate_richness/s3_richness_test",
  pattern = "[.]rds$",
  full.names = TRUE
)


data_back <- list.files(
  "Data/Paper_1/data_model/data_back_new/",
  pattern = "[.]rds$",
  full.names = TRUE
)

median_richness_back <- 
  purrr::map(1:1000, 
           .progress = TRUE,
           .f = load_pred_richness_and_select) %>% 
  bind_rows(.id = "iter") %>% 
  group_by(region, age) %>% 
  summarise (
    continental_median_richness = median(richness),
    continental_richness_upp = quantile(richness, 0.975),
    continental_richness_dwn = quantile(richness, 0.025)
  )


#----------------------------------------------------------#
# 3. Visualise continental trends -------------------------
#----------------------------------------------------------# 

# All continents in one plot

median_richness_back %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
  geom_line(linewidth = 1, color = "red") + 
  geom_line(aes(x = age, y = continental_median_richness, group = iter),
    linewidth = 0.1, 
    alpha = 0.05, 
    color = "gray60"
  ) +
  geom_ribbon(aes(ymin = continental_richness_dwn, 
  ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  facet_wrap(~region) +
  theme_classic()+
  scale_x_reverse()



#----------------------------------------------------------#
# 4. Visualize trends per iteration with site-level trends --
#----------------------------------------------------------# 

##4.1. for single iteration

readr::read_rds(data_rich[[1]]) %>% summary()

### e.g. for iteration 1

plot_richness_iter(data_back,data_rich,1)


##4.2. for all iterations and summarized to one trend line

all_data_back_iters <- purrr::map(
  .progress = TRUE,
  .x  = data_back,
  .f  =  ~ {
    iters <- .x %>% 
      readr::read_rds()
    
     iters_new <- iters %>% 
       mutate(median_richness = median(richness),
              ci_lower = quantile(richness, 0.25, na.rm = TRUE),
              ci_upper = quantile(richness, 0.975, na.rm = TRUE) 
     
       )
  }
)
