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
<<<<<<< HEAD
# 2. Load  files for prediction -----
=======
# 1. Load  files for prediction -----
>>>>>>> df7a7e3fe5e9416dc270d1e13ca9639ab704128b
#----------------------------------------------------------#

standardize_richness <- 
  readr::read_rds(here("Data/Paper_1/data_estimate_richness/standardized_richness.rds"))

study3_richness_sd <- 
  readr::read_rds(here("Data/Paper_1/data_estimate_richness/study3_richness_sd.rds"))

#----------------------------------------------------------#
<<<<<<< HEAD
# 3. Model predictions -----
=======
# 2. Model predictions -----
>>>>>>> df7a7e3fe5e9416dc270d1e13ca9639ab704128b
#----------------------------------------------------------#

data_dummy_full <-
  purrr::map(
    .progress = TRUE,
    .x = standardize_richness,
    .f = ~ {
      tidyr::expand_grid(
        dplyr::distinct(., region),
        age = seq(
          min(.$age),
          max(.$age),
          length.out = 100
        )
      )
    }
    
  )

summary(standardize_richness[[1]])

<<<<<<< HEAD
## 3.1.Prediction

### 3.1.1.Load model iterations
=======
## 2.1.Prediction

### 2.1.1.Load model iterations
>>>>>>> df7a7e3fe5e9416dc270d1e13ca9639ab704128b

gam_mods <- 
  list.files(
    "Data/Paper_1/data_model/mod_iterations",
    pattern = "[.]rds$",
    full.names = TRUE
  )

<<<<<<< HEAD
##View single model

gam_mods[[1]] %>% read_rds()

### 3.1.2.Predict
=======
### 2.1.2.Predict
>>>>>>> df7a7e3fe5e9416dc270d1e13ca9639ab704128b

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


<<<<<<< HEAD
# View single prediction

=======
>>>>>>> df7a7e3fe5e9416dc270d1e13ca9639ab704128b
one <- read_rds(here("Data/Paper_1/data_model/preds/pred_1.rds"))

summary(one)

<<<<<<< HEAD
### 3.1.3.Back-transform richness
=======
### 2.1.3.Back-transform richness
>>>>>>> df7a7e3fe5e9416dc270d1e13ca9639ab704128b

data_pred_1 <- one

data_sd_1 <- study3_richness_sd[[1]]

data_pred_1 %>% 
  dplyr::left_join(data_sd_1, by = "region") %>%
  dplyr::mutate(
    richness  = estimate * sd_richness + mean_richness,
    rich_low  = conf_low * sd_richness + mean_richness,
    rich_high = conf_high * sd_richness + mean_richness,
  ) %>% 
  split(.$region)%>% 
  purrr::map(summary)

data_plot <- 
  data_pred_1 %>% 
  dplyr::left_join(data_sd_1, by = "region") %>%
  dplyr::mutate(
    richness  = estimate * sd_richness + mean_richness,
    rich_low  = conf_low * sd_richness + mean_richness,
    rich_high = conf_high * sd_richness + mean_richness,
  )



summary(standardize_richness[[1]])


preds <- 
  list.files(here::here("Data/Paper_1/data_model/preds"), pattern = "[.]rds$",full.names = TRUE ) 

out_back <-   here("Data/Paper_1/data_model/data_back")


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

<<<<<<< HEAD
#View single back-transformed iteration

data_back <- read_rds(here("Data/Paper_1/data_model/data_back/1.rds"))

print(data_back)
=======

check <- read_rds(here("Data/Paper_1/data_model/data_back/1.rds"))


View(check)
>>>>>>> df7a7e3fe5e9416dc270d1e13ca9639ab704128b

