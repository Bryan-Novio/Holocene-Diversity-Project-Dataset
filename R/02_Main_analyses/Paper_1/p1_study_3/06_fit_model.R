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
#                 ---- MODEL FITTING ----
#----------------------------------------------------------#
# 1. Setup -----
#----------------------------------------------------------#

library(tidyverse)
library(here)
library(mgcv)

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
# 2. Load richness  -----------------------------------------
#----------------------------------------------------------# 

paths <- list.files(
  "Data/Paper_1/data_estimate_richness/s3_richness",
  pattern = "[.]rds$",
  full.names = TRUE
)

#----------------------------------------------------------#
# 3. Output folders ----------
#----------------------------------------------------------#

out_dir_mod <- 
  here("Data/Paper_1/data_model/mod_iterations")

out_dir_preds <- 
  here("Data/Paper_1/data_model/preds")

#----------------------------------------------------------#
# 4. Standardize richness  ----
#----------------------------------------------------------#

standardize_richness <- purrr::map(
           .x = paths,
           .f = ~ {
             readr::read_rds(.x) %>%
               dplyr::group_by(region) %>%
               dplyr::mutate(st_richness = scale(richness)[, 1]) %>%
               dplyr::ungroup() %>%
               dplyr::mutate(
                 region = as.factor(region),
                 dataset_id = as.factor(dataset_id))
           }
         )


# Get mean and sd to back-transform 

study3_richness_sd <- purrr::map(
            .x = standardize_richness,
            .f = ~{
              .x %>% 
              dplyr::group_by(region) %>% 
              dplyr::summarise(mean_richness = mean(richness, na.rm = TRUE), 
              sd_richness = sd(richness, na.rm = TRUE)) %>% 
              dplyr::ungroup()
            }
          )

#----------------------------------------------------------#
# 5. Model fitting -----
#----------------------------------------------------------#

## 5.1. Parallel processing -----

n_available_cores <-
  parallelly::availableCores() - 1

# number of cores to use cannot be more than number of random effect levels
n_cores_to_use <-
  standardize_richness[[1]] %>%
  dplyr::distinct(dataset_id) %>%
  nrow() %>%
  {
    . - 1
  } %>%
  min(., n_available_cores)

## 5.2. Fit model -----

set.seed(19900723)

purrr::walk2(
  .progress = TRUE,
  .x = standardize_richness,
  .y = seq_along(standardize_richness),
  .f = ~ {
    out_file <- file.path(out_dir_mod, paste0("model_", .y, ".rds"))
    
    # skip if iteration already ran
    if (file.exists(out_file)) {
      message("Skipping: ", .y)
      return(NULL)
    }
    
    message("Running: ", .y)
    
    model <- fit_regression_model(
      data_source = .x,
      y_var = "st_richness",
      time_var = "age",
      group_var = "region",
      random = "intercept_reg",
      sel_k = 12,
      error_family = scat(),
      nthreads = n_cores_to_use,
      discrete = TRUE,
      control = mgcv::gam.control(
        trace = FALSE,
        maxit = 500
      )
    )
    
    readr::write_rds(model, out_file)
  }
)

# View a single iteration

one <- 
  readr::read_rds(here::here("Data/Paper_1/data_model/mod_iterations/model_1.rds"))

# Check model parameters

mod_iters <- list.files(
    "Data/Paper_1/data_model/mod_iterations",
    pattern = "[.]rds$",
    full.names = TRUE
  )

mod_param <- purrr::map(
  .progress = TRUE,
  .x = mod_iters,
  .f = ~ {
    .x %>% 
      readr::read_rds() %>% 
    mgcv::k.check()
  }
)

#----------------------------------------------------------#
# 6. Model predictions -----
#----------------------------------------------------------#

data_dummy_full <-
  purrr::map(
    .progress = TRUE,
    .x = standardize_richness,
    .f = ~ {
        tidyr::expand_grid(
          dplyr::distinct(.,region, dataset_id),
          age = seq(
            min(.$age),
            max(.$age),
            length.out = 100
          )
        )
    }
    
  )

data_dummy_full[[1]]

standardize_richness[[1]]

## 6.1.Prediction

### 6.1.1.Load model iterations
gam_mods <- 
  list.files(
    "Data/Paper_1/data_model/mod_iterations",
    pattern = "[.]rds$",
    full.names = TRUE
  )

### 6.1.2.Predict

purrr::walk2(
    .progress = TRUE,
    .x  = gam_mods,
    .y  = data_dummy_full,
    .f = ~ {
      
      out_file <- file.path(out_dir_preds, paste0("pred_", seq_along(.y), ".rds"))
      
      mods <- 
        readr::read_rds(.x)
      
      preds <-
        predict_model(
          model = mods,
          newdata =.y,
          type = "response",
          exclude_terms = "region"
        ) %>%
        as.data.frame() %>%
        tibble::as_tibble() %>%
        dplyr::relocate(
          estimate, region, age,
          .before = dplyr::everything()
        )
      
    }
  )

### 6.1.3.Back-transform richness

data_back_transform <- 
  purrr::map2(
    .x = data_pred_full,
    .y = study3_richness_sd,
    .f = ~ {
      .x %>% 
        left_join(.y, by = "region") %>% 
        dplyr::mutate(
          richness = estimate*sd_richness + mean_richness,
          rich_low = conf_low*sd_richness + mean_richness,
          rich_high = conf_high*sd_richness + mean_richness) 
      
    }
  )

#----------------------------------------------------------#
# 7. Save  files for prediction -----
#----------------------------------------------------------#

readr::write_rds(standardize_richness,here("Data/Paper_1/data_estimate_richness/standardized_richness.rds"))

readr::write_rds(study3_richness_sd,here("Data/Paper_1/data_estimate_richness/study3_richness_sd.rds"))




