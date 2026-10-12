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
#                 ---- MODEL PREDICTION ----
#----------------------------------------------------------#
# 1. Setup -----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(tictoc)

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
# 2. Load  files for prediction -----
#----------------------------------------------------------#

standardize_richness <- 
  readr::read_rds(here("Data/Paper_1/data_estimate_richness/standardized_richness.rds"))

study3_richness_sd <- 
  readr::read_rds(here("Data/Paper_1/data_estimate_richness/study3_richness_sd.rds"))

#----------------------------------------------------------#
# 3. Model predictions -----
#----------------------------------------------------------#

<<<<<<< HEAD:R/02_Main_analyses/Paper_1/p1_study_3/07_predict_model.R
out_dir_preds <- 
  here("Data/Paper_1/data_model/preds_50")
=======
vec_ages <-  seq(0, 12e3, 100) 
>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85:R/02_Main_analyses/Paper_1/p1_study_3/06_predict_model.R

data_dummy_full <-
  purrr::map(
    .progress = TRUE,
    .x = standardize_richness,
    .f = ~ {
      
      age_limits <- 
        .x %>% 
        dplyr::group_by(region) %>% 
        dplyr::summarise(
          min_age = min(age),
          max_age = max(age)
        )
      
      tidyr::expand_grid(
        region = unique(.x$region),
        age = vec_ages, 
      ) %>% 
        dplyr::left_join(
        age_limits,
        by = "region"
      ) %>% 
        dplyr::filter(
          age >= min_age,
          age <= max_age
        ) %>% 
        dplyr::select(-c(min_age, max_age))
    }
    
  )

summary(standardize_richness[[1]])

## 3.1.Prediction

### 3.1.1.Load model iterations

gam_mods <- 
  list.files(
<<<<<<< HEAD:R/02_Main_analyses/Paper_1/p1_study_3/07_predict_model.R
    "Data/Paper_1/data_model/mod_iters_50",
=======
    "Data/Paper_1/data_model/mod_iterations_new",
>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85:R/02_Main_analyses/Paper_1/p1_study_3/06_predict_model.R
    pattern = "[.]rds$",
    full.names = TRUE
  )

##View single model

gam_mods[[1]] %>% read_rds()

### 3.1.2.Predict

n <- length(gam_mods)

tic()


for (i in seq_along(gam_mods)) {
  
  tic(paste("Iteration", i, "of", n))
  
  mod_path <- gam_mods[[i]]
  data_i   <- data_dummy_full[[i]]
  
  out_file <- file.path(out_dir_preds, paste0("pred_", i, ".rds"))
  
  if (file.exists(out_file)) {
    toc(log = TRUE)
    next
  }
  
  mods <- read_rds(mod_path)
  
  preds <-
    predict_model(
      model = mods,
      newdata = data_i,
      type = "response",
      exclude_terms = "dataset_id"
    ) %>%
    as_tibble() %>%
    relocate(estimate, region, age, .before = everything())
  
  preds %>% 
    write_rds(., file = out_file, compress = "gz")
  
  rm(mods, preds)
  if (i %% 10 == 0) gc(FALSE)
  
  toc(log = TRUE)
}

toc()  


# View single prediction

<<<<<<< HEAD:R/02_Main_analyses/Paper_1/p1_study_3/07_predict_model.R
one <-
  read_rds(here("Data/Paper_1/data_model/preds/pred_1.rds"))
=======
one <- read_rds(here("Data/Paper_1/data_model/preds_new/pred_1.rds"))
>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85:R/02_Main_analyses/Paper_1/p1_study_3/06_predict_model.R

summary(one)

### 3.1.3.Back-transform richness


preds <- 
<<<<<<< HEAD:R/02_Main_analyses/Paper_1/p1_study_3/07_predict_model.R
  list.files(here::here("Data/Paper_1/data_model/preds_50"), pattern = "[.]rds$",full.names = TRUE ) 

out_back <- here("Data/Paper_1/data_model/data_back_2")
=======
  list.files(here::here("Data/Paper_1/data_model/preds_new"), pattern = "[.]rds$",full.names = TRUE ) 

out_back <-   here("Data/Paper_1/data_model/data_back_new")
>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85:R/02_Main_analyses/Paper_1/p1_study_3/06_predict_model.R


for (i in seq_along(preds)) {
  
  # 1. Read the data
  path <- preds[i]
  data_back <- readr::read_rds(path)
  
  # 2. Join and back-transform
  data_back <- data_back %>%
    dplyr::left_join(study3_richness_sd[[i]], by = "region") %>%
    dplyr::mutate(
      richness  = estimate * sd_richness + mean_richness,
      rich_low  = conf_low * sd_richness + mean_richness,
      rich_high = conf_high * sd_richness + mean_richness
    )
  # 3. Save sd for each iteration
  readr::write_rds(
    data_back, 
    file = stringr::str_glue("{out_back}/{i}.rds")
  )
}


#View single back-transformed iteration

<<<<<<< HEAD:R/02_Main_analyses/Paper_1/p1_study_3/07_predict_model.R
data_back <- 
  read_rds(here("Data/Paper_1/data_model/data_back_2/1.rds"))
=======
data_back <- read_rds(here("Data/Paper_1/data_model/data_back_new/1.rds"))
>>>>>>> 1b6ed01ca4d1e189a3d9a2638bcdbace616e9e85:R/02_Main_analyses/Paper_1/p1_study_3/06_predict_model.R
