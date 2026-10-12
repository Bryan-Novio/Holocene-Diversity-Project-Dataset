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

<<<<<<< HEAD:R/02_Main_analyses/Paper_1/p1_study_3/08_visualize_trend.R
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


write_csv(median_richness_back,here("Data/Paper_1/data_model/model_csvs/S3_Preds.csv"))


median_richness_back <- read_csv(here("Data/Paper_1/data_model/model_csvs/S3_Preds.csv"))


##2.2. Load richness datasets

data_rich <-
  list.files(
  "Data/Paper_1/data_estimate_richness/richness_iters",
=======
data_rich <- list.files(
  "Data/Paper_1/data_estimate_richness/s3_richness_test",
>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85:R/02_Main_analyses/Paper_1/p1_study_3/07_visualize_trend.R
  pattern = "[.]rds$",
  full.names = TRUE
)

<<<<<<< HEAD:R/02_Main_analyses/Paper_1/p1_study_3/08_visualize_trend.R
=======
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
  (
    continental_median_richness = median(richness),
    continental_richness_upp = quantile(richness, 0.975),
    continental_richness_dwn = quantile(richness, 0.025)
  )

>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85:R/02_Main_analyses/Paper_1/p1_study_3/07_visualize_trend.R
#----------------------------------------------------------#
# 3. Visualise continental trends -------------------------
#----------------------------------------------------------# 

# All continents in one plot

median_richness_back %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
<<<<<<< HEAD:R/02_Main_analyses/Paper_1/p1_study_3/08_visualize_trend.R
  geom_line(linewidth = 2, color = "red") + 
=======
  geom_line(linewidth = 1, color = "red") + 
  geom_line(aes(x = age, y = continental_median_richness, group = iter),
    linewidth = 0.1, 
    alpha = 0.05, 
    color = "gray60"
  ) +
>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85:R/02_Main_analyses/Paper_1/p1_study_3/07_visualize_trend.R
  geom_ribbon(aes(ymin = continental_richness_dwn, 
  ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  facet_wrap(~region) +
  theme_classic()+
  scale_x_reverse()



#Europe


eu_s3 <- median_richness_back %>% 
  filter(region == "Europe") %>% 
  rename(est= continental_median_richness,
         low = continental_richness_dwn,
         upp = continental_richness_upp)

gen_trend_eu_s3 <- 
  plot_trend_general(eu_s3 , "darkorchid3") 

gen_trend_eu_s3  + coord_cartesian(ylim = c(15,36))



# Asia

as_s3 <- 
  median_richness_back %>% 
  filter(region == "Asia") %>% 
  rename(est= continental_median_richness,
         low = continental_richness_dwn,
         upp = continental_richness_upp)

gen_trend_as_s3 <- 
  plot_trend_general(as_s3 , "red") 

gen_trend_as_s3  + coord_cartesian(ylim = c(15,23))

# N. America

<<<<<<< HEAD:R/02_Main_analyses/Paper_1/p1_study_3/08_visualize_trend.R

na_s3 <- median_richness_back %>% 
=======
median_richness_back %>% 
>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85:R/02_Main_analyses/Paper_1/p1_study_3/07_visualize_trend.R
  filter(region == "North America") %>% 
  rename(est= continental_median_richness,
         low = continental_richness_dwn,
         upp = continental_richness_upp)

gen_trend_as_s3 <- 
  plot_trend_general(na_s3 , "orange") 

gen_trend_as_s3  + coord_cartesian(ylim = c(12,23))


#----------------------------------------------------------#
# 4. Visualize trends per iteration with site-level trends --
#----------------------------------------------------------# 

##4.1. for single iteration

readr::read_rds(data_rich[[1]]) %>% summary()

### e.g. for iteration 1

plot_richness_iter(data_back,data_rich,1)

<<<<<<< HEAD:R/02_Main_analyses/Paper_1/p1_study_3/08_visualize_trend.R
##4.2. for all iterations and summarized to one trend line
=======
##2.2. load all iterations and summarized to one trend line
>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85:R/02_Main_analyses/Paper_1/p1_study_3/07_visualize_trend.R

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
