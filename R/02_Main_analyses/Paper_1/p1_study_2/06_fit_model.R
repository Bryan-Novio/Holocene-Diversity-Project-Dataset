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

#  Convert dataset_id as random factor
richness_data <-
  richness_data %>%              
  mutate(dataset_id = as_factor(dataset_id))

p1 <-
  ggplot2::ggplot(
    richness_data,
    ggplot2::aes(x = age, y = richness, color = dataset_id)
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line(
    linetype = "dashed",
    alpha = 0.5
  ) +
  ggplot2::labs(
    title = "Pollen Richness vs. Age (cal. yrs. BP) ",
    y = "Pollen Richness", x = "Age"
  )

#----------------------------------------------------------#
# 3. Model fitting -----
#----------------------------------------------------------#

## 3.1. Parallel processing -----
sel_cluster_type <-
  ifelse(
    .Platform["OS.type"] == "unix",
    "FORK",
    "PSOCK"
  )

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

cl <-
  parallel::makeCluster(
    n_cores_to_use,
    type = sel_cluster_type
  )

## 3.2. Fit model -----

set.seed(19900723)
gam_1 <-
  fit_regression_model(
    data_source = richness_data,
    y_var = "richness",
    time_var = "age",
    group_var = "dataset_id",
    random = "both",
    sel_k = 14, #lowered from 25
    error_family = stats::poisson(link = "log"),
    cluster = cl,
    control = mgcv::gam.control(
      trace = TRUE,
      maxit = 500
    )
  )

## 3.3. Stop cluster -----
parallel::stopCluster(cl)


#----------------------------------------------------------#
# 4. Model prediction -----
#----------------------------------------------------------#

data_dummy_full <-
  tidyr::expand_grid(
    dataset_id = unique(richness_data$dataset_id),
    age = seq(
      min(richness_data$age),
      max(richness_data$age),
      length.out = 100
    )
  )

data_dummy_general <-
  tidyr::expand_grid(
    age = seq(
      min(richness_data$age),
      max(richness_data$age),
      length.out = 100
    )
  )

data_pred_full <-
  predict_model(
    model = gam_1,
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
    model = gam_1,
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

# gam by dataset_id
p1 +
  ggplot2::geom_ribbon(
    data = data_pred_general,
    ggplot2::aes(
      x = age,
      y = estimate,
      ymin = conf_low,
      ymax = conf_high
    ),
    alpha = 0.2
  ) +
  ggplot2::geom_line(
    data = data_pred_general,
    ggplot2::aes(x = age, y = estimate),
    linewidth = 2
  ) +
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
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, max(richness_data$richness) + 5)
  )

#----------------------------------------------------------#
# 6. Save model as RDS files --
#----------------------------------------------------------# 

write_rds(gam_1,here("Data/Paper_1/data_model/gam_1.rds"))

