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
          dplyr::distinct(.x, region),
          age = seq(
            1000,
            20000,
            length.out = 100
          )
        )
    }
    
  )

data_dummy_full[[1]]

summary(data_dummy_full[[1]])

summary(standardize_richness[[1]])

## 6.1.Prediction

### 6.1.1.Load model iterations

gam_mods <- 
  list.files(
    "Data/Paper_1/data_model/mod_iterations",
    pattern = "[.]rds$",
    full.names = TRUE
  )

### 6.1.2.Predict

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


one <- read_rds(here("Data/Paper_1/data_model/preds/pred_.rds"))

summary(one)

### 6.1.3.Back-transform richness

data_pred_1 <- preds

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


check <- read_rds(here("Data/Paper_1/data_model/data_back/1.rds"))


View(check)
#----------------------------------------------------------#
# 7. Save  files for prediction -----
#----------------------------------------------------------#

readr::write_rds(standardize_richness,here("Data/Paper_1/data_estimate_richness/standardized_richness.rds"))

readr::write_rds(study3_richness_sd,here("Data/Paper_1/data_estimate_richness/study3_richness_sd.rds"))

