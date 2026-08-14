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

##2.0. Load back transformed richness datasets

data_back <- 
  list.files(
    "Data/Paper_1/data_model/data_back_2",
    pattern = "[.]rds$",
    full.names = TRUE)


## 2.1. Batch loading of back-transformed richness dataset (1000)

### 2.1.1. Batches of 50

batch_size <- 50

file_groups <- 
  split(1:1000, ceiling(1:1000 / batch_size))

### 2.1.2. Initialize an empty list to collect batches

all_batches <- list()

### 2.1.3. Process each batch

for (i in seq_along(file_groups)) {
  
  all_batches[[i]] <-
    file_groups[[i]] %>%
    map(load_pred_richness_and_select) %>%
    list_rbind()
  
  gc()
}

### 2.1.4. Combine all batches

combined_data <-
  list_rbind(all_batches)


rm(all_batches)
gc()

### 2.1.5. Load all batches

median_richness_back <- 
  combined_data %>%
  group_by(region, age) %>%
  summarise(
    continental_median_richness = median(richness, na.rm = TRUE),
    continental_richness_upp = quantile(richness, 0.975, na.rm = TRUE),
    continental_richness_dwn = quantile(richness, 0.025, na.rm = TRUE),
    .groups = "drop"
  )

rm(combined_data)
gc()


##2.2. Load richness datasets

data_rich <-
  list.files(
  "Data/Paper_1/data_estimate_richness/richness_iters",
  pattern = "[.]rds$",
  full.names = TRUE
)



#----------------------------------------------------------#
# 3. Visualise continental trends -------------------------
#----------------------------------------------------------# 

# All continents in one plot

median_richness_back %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
  geom_line(linewidth = 2, color = "red") + 
  geom_ribbon(aes(ymin = continental_richness_dwn, 
              ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  facet_wrap(~region) +
  theme_classic()+
  scale_x_reverse()


#Europe

median_richness_back %>% 
  filter(region == "Europe") %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
  geom_line(linewidth = 4, color = "red") + 
  geom_ribbon(aes(ymin = continental_richness_dwn, 
                  ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  labs(x = "Age(cal yr BP)" , y = "Richness") +
  theme_classic()+
  scale_x_reverse()

# Asia

median_richness_back %>% 
  filter(region == "Asia") %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
  geom_line(linewidth = 4, color = "red") + 
  geom_ribbon(aes(ymin = continental_richness_dwn, 
                  ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  labs(x = "Age(cal yr BP)" , y = "Richness") +
  theme_classic()+
  scale_x_reverse()

# N. America


median_richness_back %>% 
  filter(region == "North America") %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
  geom_line(linewidth = 4, color = "red") + 
  geom_ribbon(aes(ymin = continental_richness_dwn, 
                  ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  labs(x = "Age(cal yr BP)" , y = "Richness") +
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

