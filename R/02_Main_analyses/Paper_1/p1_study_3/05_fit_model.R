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
# 2. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness_data_asia <- 
  read_csv(here("Data/Paper_1/data_estimate_richness/study3_richness_asia.csv"))

richness_data_europe <- 
  read_csv(here("Data/Paper_1/data_estimate_richness/study3_richness_europe.csv"))

richness_data_namerica <- 
  read_csv(here("Data/Paper_1/data_estimate_richness/study3_richness_namerica.csv"))

#  add regions to each dataset

richness_data_asia <-
  richness_data_asia %>%              
  mutate(region = "asia") 

richness_data_europe <-
  richness_data_europe %>% 
  mutate(region = "europe")

richness_data_namerica <-
  richness_data_namerica %>%  
  mutate(region = "namerica") 

#bind all dataframes with richness estimate, convert dataset_id as random factor and add regions

study3_richness <- 
  bind_rows(richness_data_asia,richness_data_europe, richness_data_namerica) %>%
  mutate(dataset_id = as_factor(dataset_id))

# standardize diversity use scale():

study3_richness_z_scores <- 
  study3_richness %>% 
  group_by(region) %>% 
  mutate(st_richness = scale(richness)) %>%
  ungroup() %>% 
  mutate(st_richness = st_richness[,1]) %>% 
  mutate(region = as_factor(region))

# Get mean and sd to back-transform 

study3_richness_standard <-
  study3_richness %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarise(mean_richness = mean(richness, na.rm = TRUE), 
                   sd_richness = sd(richness, na.rm = TRUE)) %>% 
  dplyr::ungroup()



#----------------------------------------------------------#
# 3. Model fitting -----
#----------------------------------------------------------#

## 3.1. Parallel processing -----

n_available_cores <-
  parallelly::availableCores() - 1

# number of cores to use cannot be more than number of random effect levels
n_cores_to_use <-
  study3_richness_z_scores %>%
  dplyr::distinct(dataset_id) %>%
  nrow() %>%
  {
    . - 1
  } %>%
  min(., n_available_cores)

## 3.2. Fit model -----

set.seed(19900723)

gam_s3 <-
  fit_regression_model(
    data = study3_richness_z_scores,
    y_var = "st_richness",
    time_var = "age",
    group_var = "region",
    random = "study3",
    sel_k = 12, 
    error_family = scat(),
    nthreads = n_cores_to_use,
    discrete = TRUE,
    control = mgcv::gam.control(
      trace = TRUE,
      maxit = 500
    )
  )

## 3.3. Save model as RDS files --

write_rds(gam_s3,here("Data/Paper_1/data_model/gam_3.rds"))

#----------------------------------------------------------#
# 4. Model prediction -----
#----------------------------------------------------------#

data_dummy_full <-
  tidyr::expand_grid(
    study3_richness_z_scores %>% 
      distinct(region),
    age = seq(
      min(study3_richness_z_scores$age),
      max(study3_richness_z_scores$age),
      length.out = 100
    )
  )

data_pred_full <-
  predict_model(
    model = gam_s3,
    newdata = data_dummy_full,
    type = "response",
    exclude_terms = "dataset_id"
  ) %>%
  as.data.frame() %>%
  tibble::as_tibble() %>%
  dplyr::relocate(
    estimate, region, age,
    .before = dplyr::everything()
  )

data_richness_btr <- 
  data_pred_full %>% 
  left_join(study3_richness_standard, by = "region") %>% 
  mutate(
    richness = estimate*sd_richness + mean_richness,
    rich_low = conf_low*sd_richness + mean_richness,
    rich_high = conf_high*sd_richness + mean_richness) 
  

#----------------------------------------------------------#
# 4. Visualization -----
#----------------------------------------------------------#

##general plot

p <-
  ggplot2::ggplot(
  ) +
  ggplot2::labs(
    y = "Pollen Richness", x = "cal yr BP") +
  ggplot2::theme_classic(
    
  )+
  ggplot2::theme(legend.position = "none",
                 plot.title = element_text(color = "#2a707f"),
                 axis.title = element_text(color = "#2a707f", size = 14),
                 axis.text  = element_text(color = "#2a707f", size = 24),
                 axis.ticks = element_line(color = "#2a707f"),
                 axis.line  = element_line(color = "#2a707f", linewidth = 1)
  )

#  4.1. Plot predictions for each region -----

#show continental trend in one figure

p1 <- p +
  ggplot2::facet_wrap(~ region, dir = 'rt', ncol = 1, strip.position = 'right') +
  ggplot2::geom_ribbon(
    data = data_richness_btr,
    ggplot2::aes(
      x = age,
      y = richness,
      ymin = rich_low,
      ymax = rich_high,
      fill = as.factor(region)
    ),
    alpha = 0.3
  ) +
  ggplot2::geom_line(
    data = data_richness_btr,
    ggplot2::aes(x = age, y = richness,color = region),
    linewidth = 1
  ) +
  ggplot2::theme(legend.position = "none",
                 axis.text  = element_text(size = 5),
                 strip.text = element_text(
                   size = 10,
                   color = "#2a707f"
                 ),
                 strip.background = element_rect(
                   color = "#2a707f",
                   fill = NA,
                   linewidth = 0.3
                 )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(14, 24) 
  ) 

p1 +
  ggplot2::scale_x_reverse()


## show individual trend for each continent

asia <- data_richness_btr %>% filter(region == 'asia')
europe <- data_richness_btr %>% filter(region == 'europe')
namerica <- data_richness_btr %>% filter(region == 'namerica')

##Asia

A <- p +
  ggplot2::geom_ribbon(
    data = asia,
    ggplot2::aes(
      x = age,
      y = richness,
      ymin = rich_low,
      ymax = rich_high,
      fill = region
    ),
    alpha = 0.1
  ) +
  ggplot2::geom_line(
    data = asia,
    ggplot2::aes(x = age, y = richness),
    linewidth = 2, color = 'red'
  ) +
  ggplot2::theme(legend.position = "none",
                 axis.text  = element_text(size = 9),
                 strip.text = element_text(
                   size = 10,
                   color = "#2a707f"
                 ),
                 strip.background = element_rect(
                   color = "#2a707f",
                   fill = NA,
                   linewidth = 0.3
                 )
  ) +
  ggplot2::coord_cartesian(ylim = c(14, 23) 
  ) + 
  ggplot2::scale_x_reverse()

A

#Europe

E <- p +
  ggplot2::geom_ribbon(
    data = europe,
    ggplot2::aes(
      x = age,
      y = richness,
      ymin = rich_low,
      ymax = rich_high,
      fill = region
    ),
    alpha = 0.1
  ) +
  ggplot2::geom_line(
    data = europe,
    ggplot2::aes(x = age, y = richness),
    linewidth = 2, color = 'purple'
  ) +
  ggplot2::theme(legend.position = "none",
                 axis.text  = element_text(size = 9),
                 strip.text = element_text(
                   size = 10,
                   color = "#2a707f"
                 ),
                 strip.background = element_rect(
                   color = "#2a707f",
                   fill = NA,
                   linewidth = 0.3
                 )
  ) +
  ggplot2::coord_cartesian(ylim = c(14, 22) 
  ) + 
  ggplot2::scale_x_reverse()

E

##NAmerica

N <- p +
  ggplot2::geom_ribbon(
    data = namerica,
    ggplot2::aes(
      x = age,
      y = richness,
      ymin = rich_low,
      ymax = rich_high,
      fill = region
    ),
    alpha = 0.1
  ) +
  ggplot2::geom_line(
    data = namerica,
    ggplot2::aes(x = age, y = richness),
    linewidth = 2, color = 'orange'
  ) +
  ggplot2::theme(legend.position = "none",
                 axis.text  = element_text(size = 9),
                 strip.text = element_text(
                   size = 10,
                   color = "#2a707f"
                 ),
                 strip.background = element_rect(
                   color = "#2a707f",
                   fill = NA,
                   linewidth = 0.3
                 )
  ) +
  ggplot2::coord_cartesian(ylim = c(16, 19) 
  ) + 
  ggplot2::scale_x_reverse()

N

##combine continental trends into single plot

library(patchwork)

A + E + N

