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

out_dir_preds <- 
  here("Data/Paper_1/data_model/preds_new")

vec_ages <-  seq(0, 12e3, 100) 

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



summary(standardize_richness[["1"]])

standardize_richness[["10"]]

names(standardize_richness)[10]


## 3.1.Prediction

### 3.1.1.Load model iterations

gam_mods <- 
  list.files( "Data/Paper_1/data_model/mod_iterations_new",
    pattern = "[.]rds$",
    full.names = TRUE) %>% 
  rlang::set_names(
    nm = str_extract(.,"(?<=model_)\\d+(?=\\.rds$)")
  )

##View single model

gam_mods[1:10]

gam_mods[["100"]] %>% read_rds()

### 3.1.2.Predict

n <- length(gam_mods)

tic()


for (i in seq_along(gam_mods)) {
  
  iter_name <- names(gam_mods)[i] 
  
  tic(paste("Iteration", iter_name , "of", n))
  
  mod_path <- gam_mods[[iter_name]]
  data_i   <- data_dummy_full[[iter_name]]
  
  out_file <- file.path(out_dir_preds, paste0("pred_", iter_name, ".rds"))
  
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

two <- read_rds(here("Data/Paper_1/data_model/preds_new/pred_102.rds"))

summary(two)

### 3.1.3.Back-transform richness


names_preds <- 
  list.files( file.path(out_dir_preds),
              pattern = "[.]rds$",
              full.names = TRUE) %>% 
  rlang::set_names(
    nm = str_extract(.,"(?<=pred_)\\d+(?=\\.rds$)")
  )

out_back <- here("Data/Paper_1/data_model/data_back_new")


for (i in seq_along(names_preds)) {
  
  iter_name <- names(names_preds)[i] 
  
  # 1. Read the data
  data_back <- readr::read_rds(names_preds[[iter_name]])
  
  # 2. Join and back-transform
  data_back_trans <- 
    data_back %>%
    dplyr::left_join(study3_richness_sd[[iter_name]], by = "region") %>%
    dplyr::mutate(
      richness  = estimate * sd_richness + mean_richness,
      rich_low  = conf_low * sd_richness + mean_richness,
      rich_high = conf_high * sd_richness + mean_richness
    )
  # 3. Save sd for each iteration
  readr::write_rds(
    data_back_trans, 
    file = stringr::str_glue("{out_back}/{iter_name}.rds")
  )
}


#View single back-transformed iteration


data_back <- read_rds(here("Data/Paper_1/data_model/data_back_new/1.rds"))


