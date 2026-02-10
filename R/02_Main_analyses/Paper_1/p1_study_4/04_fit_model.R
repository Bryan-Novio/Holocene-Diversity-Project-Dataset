#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 4: Bhatta et al
#
#
#                       
#                          2023
# Asia, site-based richness (dataset_id,age)
# nonbinned  - rarefy 300 
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
# 2. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness_data4 <- 
  read_rds(here("Data/Paper_1/data_estimate_richness/study4_richness.csv"))

p <-
  ggplot2::ggplot(
    richness_data4,
    ggplot2::aes(x = age, y = richness)
  ) +
  ggplot2::labs(y = "Pollen Richness", x = "Age"
  ) +
  ggplot2::theme_classic(
  )+
  ggplot2::theme(legend.position = "none",
                 plot.title = element_text(color = "#2a707f"),
                 axis.title = element_text(color = "#2a707f", size = 22),
                 axis.text  = element_text(color = "#2a707f", size = 6),
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
  richness_data4 %>%
  dplyr::distinct(dataset_id) %>%
  nrow() %>%
  {
    . - 1
  } %>%
  min(., n_available_cores)

## 3.2. Fit model -----

set.seed(19900723)

gam_s4 <-
  fit_regression_model(
    data = richness_data4,
    y_var = "richness",
    time_var = "age",
    group_var = "dataset_id",
    random = "slope",
    sel_k = 30, 
    error_family = stats::poisson(link = "log"),
    nthreads = n_cores_to_use,
    discrete = TRUE,
    control = mgcv::gam.control(
      trace = TRUE,
      maxit = 1000
    )
  )

## 3.3. Save model as an RDS file --

write_rds(gam_s4, here("Data/Paper_1/data_model/gam_s4.rds"))

#----------------------------------------------------------#
# 4. Model prediction -----
#----------------------------------------------------------#

data_dummy_full <-
  tidyr::expand_grid(
    dataset_id = unique(richness_data4$dataset_id),
    age = seq(
      min(richness_data4$age),
      max(richness_data4$age),
      length.out = 100
    )
  )

data_dummy_general <-
  tidyr::expand_grid(
    age = seq(
      min(richness_data4$age),
      max(richness_data4$age),
      length.out = 100
    )
  )

data_pred_full <-
  predict_model(
    model = gam_s4,
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
    model = gam_s4,
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
# 4. Visualization -----
#----------------------------------------------------------#

## 4.1. Plot predictions for individual series-----

p +
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
    ggplot2::aes(x = age, y = estimate, color = dataset_id),
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
    ylim = c(0, max(richness_data4$richness) + 5)
  )

## 4.2. Plot general trend-----

p +
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
    linewidth = 4,
    color = "black"
  ) +
  ggplot2::theme(
    legend.position = "none",
    axis.text  = element_text(color = "#2a707f", size = 24,  hjust = 0.8)
  ) +
  ggplot2::coord_cartesian(
    ylim = c(10.3, 17),
    xlim = c(0, 12000)
    ) 
