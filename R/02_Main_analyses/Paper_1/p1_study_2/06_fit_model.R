#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 2: Simova et al
#
#                       
#                          2023
# North America, site-based richness (dataset_id,age, 
# 1000 bins - rarefy 400 
#
#               ---- MODEL FITTING ----
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
# 2. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness_data <- 
  read_csv(here("Data/Paper_1/data_estimate_richness/study2_richness.csv"))

hist(richness_data$richness)

#  Convert dataset_id as random factor

richness_data <-
  richness_data %>%              
  mutate(dataset_id = as_factor(dataset_id))

p1 <-
  ggplot2::ggplot(
    richness_data,
    ggplot2::aes(x = age, y = richness)
  ) +
  ggplot2::labs(
    y = "Pollen Richness", x = "Age (cal yr BP)"
  )+
  ggplot2::theme_classic(
    
  )+
  ggplot2::theme(legend.position = "none",
                 plot.title = element_text(color = "#2a707f"),
                 axis.title = element_text(color = "#2a707f", size = 18),
                 axis.text  = element_text(color = "#2a707f", size = 18),
                 axis.ticks = element_line(color = "#2a707f"),
                 axis.line  = element_line(color = "#2a707f", linewidth = 1)
                 )


#----------------------------------------------------------#
# 3. Model fitting -----
#----------------------------------------------------------#

## 3.1. Parallel processing -----

n_available_cores <-
  parallelly::availableCores() - 1

# number of cores to use cannot be more than number of random effect levels
n_cores_to_use <-
  richness_data %>%
  dplyr::distinct(dataset_id) %>%
  nrow() %>%
  {
    . - 1
  } %>%
  min(., n_available_cores)


## 3.2. Fit model -----

set.seed(19900723)

gam_s2 <-                               #method = REML
  fit_regression_model(
    data_source = richness_data,
    y_var = "richness",
    time_var = "age",
    group_var = "dataset_id",
    random = "slope_rand_id_2",
    sel_k = 10, 
    error_family = stats::gaussian(),
    nthreads = n_cores_to_use,
    discrete = TRUE,
    control = mgcv::gam.control(
      trace = TRUE,
      maxit = 500
    )
  )

gam_check <- gam.check(gam_s2)
summary(gam_s2)

write_csv(gam_check,here("Outputs/Paper_1/study2_gam-check.csv"))

## 3.3. Save model as RDS files --

write_rds(gam_1,here("Data/Paper_1/data_model/gam_1_na.rds"))


gam_1 <- read_rds(here("Data/Paper_1/data_model/gam_1_na.rds"))

#----------------------------------------------------------#
# 4. Model prediction -----
#----------------------------------------------------------#

data_dummy_full <-
  tidyr::expand_grid(
    dataset_id = unique(richness_data$dataset_id),
    age = seq(
      min(richness_data$age),
      max(richness_data$age),
      length.out = 1000
    )
  )

data_dummy_general <-
  tidyr::expand_grid(
    age = seq(
      min(richness_data$age),
      max(richness_data$age),
      length.out = 1000
    )
  )

data_pred_full <-
  predict_model(
    model = gam_s2,
    newdata = data_dummy_full,
    type = "response"
  ) %>%
  as.data.frame() %>%
  tibble::as_tibble() %>%
  dplyr::relocate(
    estimate, dataset_id, age,
    .before = dplyr::everything()
  )

data_pred_general <-
  predict_model(
    model = gam_s2,
    newdata = data_dummy_general,
    type = "response",
    exclude_terms = "dataset_id"
  ) %>%
  as.data.frame() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    dataset_id = NA_character_
  ) %>%
  dplyr::relocate(
    estimate,age,
    .before = dplyr::everything()
  )

#----------------------------------------------------------#
# 5. Visualization -----
#----------------------------------------------------------#

#  4.1. Plot predictions for individual series-----

p1 +
  ggplot2::facet_wrap(~ dataset_id) +
  ggplot2::geom_ribbon(
    data = data_pred_full,
    ggplot2::aes(
      x = age,
      y = estimate,
      ymin = conf_low,
      ymax = conf_high,
      fill = dataset_id
    ),
    alpha = 0.1
  ) +
  ggplot2::geom_line(
    data = data_pred_full,
    ggplot2::aes(x = age, y = estimate,color = dataset_id),
    linewidth = 1
  ) +
  ggplot2::theme(legend.position = "none",
                 axis.text  = element_text(size = 5),
                 strip.text = element_text(
                   size = 6,
                   color = "#2a707f"
                 ),
                 strip.background = element_rect(
                   color = "#2a707f",
                   fill = NA,
                   linewidth = 0.3
                 )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, max(richness_data$richness) + 5)
  )

# 4.2. Plot general trend-----

p1 +
  ggplot2::geom_ribbon(
    data = data_pred_general,
    ggplot2::aes(
      x = age,
      y = estimate,
      ymin = conf_low,
      ymax = conf_high
      ),
      fill = "gray",
      alpha = 0.1
  ) +
  ggplot2::geom_line(
    data = data_pred_general,
    ggplot2::aes(x = age, y = estimate),
    linewidth = 2,
    color = "black"
  )  +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::coord_cartesian(
    ylim = c(6,14)
  ) +
  ggplot2::scale_x_reverse() +
  ggplot2::geom_vline(xintercept = 9500, linetype = "dashed", color ="black")


 

  