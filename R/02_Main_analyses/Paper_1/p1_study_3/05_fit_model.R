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
library(mvgam)

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

richness_data_eu <- 
  read_csv(here("Data/Paper_1/data_estimate_richness/study3_richness_eu.csv"))

richness_data_na <- 
  read_csv(here("Data/Paper_1/data_estimate_richness/study3_richness_na.csv"))

#  Convert dataset_id as random factor
richness_data_eu <-
  richness_data_eu %>%              
  mutate(dataset_id = as_factor(dataset_id))

min(richness_data_eu$age)

richness_data_na <-
  richness_data_na %>%              
  mutate(dataset_id = as_factor(dataset_id))


n_datasets <- length(unique(richness_data_eu$dataset_id))
my_palette <- seq_gradient_pal("#d0a053", "#eacdaa")(seq(0, 1, length.out = n_datasets))

p2 <-
  ggplot2::ggplot(
    richness_data_eu,
    ggplot2::aes(x = age, y = richness)
  ) +
  ggplot2::labs(
    y = "Pollen Richness", x = "Age") +
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
  richness_data_na %>%
  dplyr::distinct(dataset_id) %>%
  nrow() %>%
  {
    . - 1
  } %>%
  min(., n_available_cores)

## 3.2. Fit model -----

set.seed(19900723)
gam_2 <-
  fit_regression_model(
    data = richness_data_na,
    y_var = "richness",
    time_var = "age",
    group_var = "dataset_id",
    random = "slope",
    sel_k = 20, 
    error_family = stats::poisson(link = "log"),
    nthreads = n_cores_to_use,
    discrete = TRUE,
    control = mgcv::gam.control(
      trace = TRUE,
      maxit = 500
    )
  )

## 3.3. Save model as RDS files --

write_rds(gam_2,here("Data/Paper_1/data_model/gam_2_na.rds"))
gam_2_eu <- read_rds(here("Data/Paper_1/data_model/gam_2_eu.rds"))
gam_2_na <- read_rds(here("Data/Paper_1/data_model/gam_2_na.rds"))

summary(gam_2_eu)
summary(gam_2_na)

gam.check(gam_2_eu)
gam.check(gam_2_na)

AIC(gam_2_eu)
AIC(gam_2_na)

#----------------------------------------------------------#
# 4. Model prediction -----
#----------------------------------------------------------#

data_dummy_full <-
  tidyr::expand_grid(
    dataset_id = unique(richness_data_eu$dataset_id),
    age = seq(
      min(richness_data_na$age),
      max(richness_data_na$age),
      length.out = 10
    )
  )

data_dummy_general <-
  tidyr::expand_grid(
    age = seq(
      min(richness_data_na$age),
      max(richness_data_na$age),
      length.out = 10
    )
  )

data_pred_full <-
  predict_model(
    model = gam_2_eu,
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
    model = gam_2_na,
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

#  4.1. Plot predictions for individual series-----
p2 +
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
    ylim = c(0, max(richness_data_eu$richness) + 5)
  )

# 4.2. Plot general trend-----
p2 +
  ggplot2::geom_ribbon(
    data = data_pred_general,
    ggplot2::aes(
      x = age,
      y = estimate,
      ymin = conf_low,
      ymax = conf_high
    ),
    fill = "#2a707f",
    alpha = 0.1
  ) +
  ggplot2::geom_line(
    data = data_pred_general,
    ggplot2::aes(x = age, y = estimate),
    linewidth = 3,
    color = "#2a707f"
  ) +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::coord_cartesian(
    ylim = c(15, 17) ,
    xlim = c(12000,0)
  )+
  ggplot2::scale_x_reverse(
    limits = c(12000, 0),
    breaks = seq(0, 12000, by = 4000)
  )


# standardize diversity use scale():

richness_data$richness %>% scale()
