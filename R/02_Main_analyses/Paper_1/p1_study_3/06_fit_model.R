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
      sel_k = 50,
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


#----------------------------------------------------------#
# 5. Save  files for prediction -----
#----------------------------------------------------------#

readr::write_rds(standardize_richness,here("Data/Paper_1/data_estimate_richness/standardized_richness.rds"))

readr::write_rds(study3_richness_sd,here("Data/Paper_1/data_estimate_richness/study3_richness_sd.rds"))

