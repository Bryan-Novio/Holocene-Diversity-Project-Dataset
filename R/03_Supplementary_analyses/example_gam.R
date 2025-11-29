#----------------------------------------------------------#
#
#
#         The Holocene Diversity Project
#
#           Example of GAM model fitting
#
#
#                       O. Mottl
#                         2025
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 1. Setup -----
#----------------------------------------------------------#

library(tidyverse)
library(mgcv)
library(mvgam)

fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "\\.R$",
    recursive = TRUE
  )

# Load the function into the global environment

source_files <-
  sapply(
    paste0("R/Functions/", fun_list, sep = ""),
    source
  )


#----------------------------------------------------------#
# 2. Get data -----
#----------------------------------------------------------#

data_example <-
  mvgam::portal_data %>%
  tibble::as_tibble() %>%
  dplyr::select(series, time, captures) %>%
  tidyr::drop_na()

p0 <-
  ggplot2::ggplot(
    data_example,
    ggplot2::aes(x = time, y = captures, color = series)
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line(
    linetype = "dashed",
    alpha = 0.5
  ) +
  ggplot2::labs(
    title = "Example time series data from the portal dataset",
    y = "Number of captures"
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
  data_example %>%
  dplyr::distinct(series) %>%
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
mod_1 <-
  fit_regression_model(
    data = data_example,
    y_var = "captures",
    time_var = "time",
    group_var = "series",
    random = "both",
    sel_k = 25,
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
    series = unique(data_example$series),
    time = seq(
      min(data_example$time),
      max(data_example$time),
      length.out = 100
    )
  )

data_dummy_general <-
  tidyr::expand_grid(
    time = seq(
      min(data_example$time),
      max(data_example$time),
      length.out = 100
    )
  )


data_pred_full <-
  predict_model(
    model = mod_1,
    newdata = data_dummy_full,
    type = "response"
  ) %>%
  as.data.frame() %>%
  tibble::as_tibble() %>%
  dplyr::relocate(
    estimate, series, time,
    .before = dplyr::everything()
  )

data_pred_general <-
  predict_model(
    model = mod_1,
    newdata = data_dummy_general,
    type = "response",
    exclude_terms = "series"
  ) %>%
  as.data.frame() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    series = NA_character_
  ) %>%
  dplyr::relocate(
    estimate, time,
    .before = dplyr::everything()
  )

#----------------------------------------------------------#
# 4. Visualization -----
#----------------------------------------------------------#

p0 +
  ggplot2::geom_ribbon(
    data = data_pred_general,
    ggplot2::aes(
      x = time,
      y = estimate,
      ymin = conf_low,
      ymax = conf_high
    ),
    alpha = 0.2
  ) +
  ggplot2::geom_line(
    data = data_pred_general,
    ggplot2::aes(x = time, y = estimate),
    linewidth = 2
  ) +
  ggplot2::geom_ribbon(
    data = data_pred_full,
    ggplot2::aes(
      x = time,
      y = estimate,
      ymin = conf_low,
      ymax = conf_high,
      fill = series
    ),
    alpha = 0.1
  ) +
  ggplot2::geom_line(
    data = data_pred_full,
    ggplot2::aes(x = time, y = estimate, color = series),
    linewidth = 1
  ) +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, max(data_example$captures) + 5)
  )
