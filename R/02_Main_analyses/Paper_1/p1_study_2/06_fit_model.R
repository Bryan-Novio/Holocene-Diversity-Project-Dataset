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
                 axis.text.x = element_text(size =15),
                 axis.text.y = element_text(size =15),
                 plot.title = element_text(color = "black"),
                 axis.title = element_text(color = "black", size = 18),
                 axis.line  = element_line(color = "black", linewidth = 1)
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

write_rds(gam_s2,here("Data/Paper_1/data_model/gam_1_na.rds"))

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
    model = gam_1 ,
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
    model = gam_1 ,
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

write_csv(data_pred_general,here("Data/Paper_1/data_model/model_csvs/S2_Preds.csv"))

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


### sites as lines

data_pred_full_id <- 
  data_pred_full %>% 
  group_by(dataset_id,age) %>% 
  summarise(median_richnes = median(estimate),
         median_low = median(conf_low),
         median_high = median(conf_high),
         age = median(age))%>% 
  distinct(dataset_id, age, median_richnes, median_low, median_high)


ggplot() +
  geom_line(data = data_pred_full_id , 
            aes(x = age, y = median_richnes), linewidth = 1)


data_pred_full_id %>% 
  ggplot(aes(x= age, y = median_richnes, group = dataset_id)) +
  geom_line(colour = "orange",alpha = 0.5) +
  ggplot2::theme(
    axis.text.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.x = element_text(size = 15, color = "black"),
    axis.title.y = element_text(size = 15, color = "black"),
    panel.background = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 1),
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.ticks.x = element_line(linewidth = 1),
    axis.ticks.y = element_line(linewidth = 1),
    axis.ticks.length.y. = unit(.25, "cm"),
    axis.ticks.length.x.bottom = unit(.25, "cm")) +
  labs(y = "Richness",  x = "Age (cal yr BP)")+
  ggplot2::scale_x_reverse() +   
  ggplot2::geom_vline(xintercept = 9500, color ="black", size = 1,alpha = 0.5, linetype = "dotted")



# 4.2. Plot general trend-----

data_pred_general <- 
  read_csv(here("Data/Paper_1/data_model/model_csvs/S2_Preds.csv"))

data_pred_s2 <- data_pred_general %>% 
  select(estimate, age, conf_low, conf_high) %>% 
  rename(est = estimate,
         low = conf_low,
         upp = conf_high)

data_pred_s2 %>%
  plot_trend_study_2()




 ggplot()+ 
  geom_ribbon(data = data_pred_s2, aes(x = age, ymin = low, ymax = upp), fill= "orange",alpha = 0.2
  ) +
  ggplot2::geom_line( data = data_pred_s2, aes(x = age, y = est),color = "darkorange", linewidth = 2) +
  ggplot2::theme(
    axis.text.x = element_text(size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
                 axis.title.x = element_text(size = 15, color = "black"),
                 axis.title.y = element_text(size = 15, color = "black"),
                 panel.background = element_blank(),
                 axis.line.y = element_line(color = "black", linewidth = 1),
                 axis.line.x = element_line(color = "black", linewidth = 1),
                 axis.ticks.x = element_line(linewidth = 1),
                 axis.ticks.y = element_line(linewidth = 1),
                 axis.ticks.length.y. = unit(.25, "cm"),
                 axis.ticks.length.x.bottom = unit(.25, "cm"))  +
  ggplot2::coord_cartesian(
    ylim = c(6,14)
  ) +
  labs(y = "Richness",  x = "Age (cal yr BP)")+
  ggplot2::scale_x_reverse() +   
  ggplot2::geom_vline(xintercept = 9500, color ="coral", size = 1,alpha = 0.5, linetype = "dotted")



#### with original
 
 s2_new
 
 ggplot()+ 
  # geom_ribbon(data =  s2_new, aes(x = age, ymin = low, ymax = upp), fill= "grey",alpha = 0.2
   #) +
  # ggplot2::geom_line( data = s2_new, aes(x = age, y = est),color = "grey40", linewidth = 2) +
geom_ribbon(data = data_pred_s2, aes(x = age, ymin = low, ymax = upp), fill= "orange",alpha = 0.2
 ) +
 ggplot2::geom_line( data = data_pred_s2, aes(x = age, y = est),color = "darkorange", linewidth = 2) +
   ggplot2::theme(
     axis.text.x = element_text(size = 15, color = "black"),
     axis.text.y = element_text(size = 15, color = "black"),
     axis.title.x = element_text(size = 15, color = "black"),
     axis.title.y = element_text(size = 15, color = "black"),
     panel.background = element_blank(),
     axis.line.y = element_line(color = "black", linewidth = 1),
     axis.line.x = element_line(color = "black", linewidth = 1),
     axis.ticks.x = element_line(linewidth = 1),
     axis.ticks.y = element_line(linewidth = 1),
     axis.ticks.length.y. = unit(.25, "cm"),
     axis.ticks.length.x.bottom = unit(.25, "cm"))  +
   ggplot2::coord_cartesian(
     ylim = c(6,14)
   ) +
   labs(y = "Richness",  x = "Age (cal yr BP)")+
   ggplot2::scale_x_reverse() +   
   ggplot2::geom_vline(xintercept = 9500, color ="coral", size = 1,alpha = 0.5, linetype = "dotted")
 

  