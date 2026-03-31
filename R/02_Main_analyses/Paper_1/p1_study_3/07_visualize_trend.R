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
#            ---- TREND VISUALIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data -----------------------------------------
#----------------------------------------------------------# 

standardize_richness <-
  read_rds(here("Data/Paper_1/data_estimate_richness/standardized_richness.rds"))

study3_richness_sd <- 
  read_rds(here("Data/Paper_1/data_estimate_richness/study3_richness_sd.rds"))

gam_mods <- 
  list.files(
    "Data/Paper_1/data_model/mod_iterations",
    pattern = "[.]rds$",
    full.names = TRUE
  )

#----------------------------------------------------------#
# 2. Load functions ------------------------------------
#----------------------------------------------------------# 

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
# 3. Model predictions -----
#----------------------------------------------------------#

data_dummy_full <-
  purrr::map(
    .x = standardize_richness,
    .f = ~ {
      richness_chr <- .x %>%
        mutate(region = as.character(region))
      
      tidyr::expand_grid(
        distinct(.,region),
        age = seq(
          min(.$age),
          max(.$age),
          length.out = 100
        )
      )
    }
    
  )

##Prediction

data_pred_full <-
  purrr::map2(
    .x  = gam_mods,
    .y  = data_dummy_full,
    .f = ~ {
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


data_back_transform <- 
  purrr::map2(
    .x = data_pred_full,
    .y = study3_richness_sd,
    .f = ~ {
      .x %>% 
        left_join(.y, by = "region") %>% 
        mutate(
          richness = estimate*sd_richness + mean_richness,
          rich_low = conf_low*sd_richness + mean_richness,
          rich_high = conf_high*sd_richness + mean_richness) 
      
    }
  )

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
                 axis.title = element_text(color = "#2a707f", size = 18),
                 axis.text  = element_text(color = "#2a707f", size = 24),
                 axis.ticks = element_line(color = "#2a707f"),
                 axis.line  = element_line(color = "#2a707f", linewidth = 1)
  )

#  4.1. Plot predictions for each region -----

#show continental trend in one figure

p1 <- purrr::map(
  .x = data_back_transform,
  .f = ~  {
    p + 
      ggplot2::facet_wrap(~ region, dir = 'rt', ncol = 1, strip.position = 'right') +
      ggplot2::geom_ribbon(
        data = .x,
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
        data = .x,
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
                     )) +
      ggplot2::coord_cartesian(
        ylim = c(14, 24))
    
  }
)



p1 +
  ggplot2::scale_x_reverse()


## show individual trend for each continent

asia <- 
  purrr::map(
    .x = data_back_transform,
    .f =  ~ {
      .x %>% 
        filter(region == 'asia')
    }
  )

europe <- 
  purrr::map(
    .x = data_back_transform,
    .f =  ~ {
      .x %>% 
        filter(region == 'europe')
    }
  )


namerica <- 
  purrr::map(
    .x = data_back_transform,
    .f =  ~ {
      .x %>% 
        filter(region == 'namerica')
    }
  )


##Asia

A <- purrr::map(
  .x = asia,
  .f = ~ {
    p +
      ggplot2::geom_ribbon(
        data = .x,
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
        data = .x,
        ggplot2::aes(x = age, y = richness),
        linewidth = 4, color = 'red'
      ) +
      ggplot2::theme(legend.position = "none",
                     axis.text  = element_text(size = 22),
                     strip.text = element_text(
                       size = 16,
                       color = "#2a707f"
                     ),
                     strip.background = element_rect(
                       color = "#2a707f",
                       fill = NA,
                       linewidth = 0.3
                     )
      ) +
      ggplot2::coord_cartesian(ylim = c(14, 19) 
      ) 
  }
) 

A + ggplot2::scale_x_reverse()

#Europe

E <- 
  purrr::map(
    .x = europe,
    .f = ~ {
      p +
        ggplot2::geom_ribbon(
          data = .x,
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
          data = .x,
          ggplot2::aes(x = age, y = richness),
          linewidth = 4, color = 'red'
        ) +
        ggplot2::theme(legend.position = "none",
                       axis.text  = element_text(size = 22),
                       strip.text = element_text(
                         size = 16,
                         color = "#2a707f"
                       ),
                       strip.background = element_rect(
                         color = "#2a707f",
                         fill = NA,
                         linewidth = 0.3
                       )
        ) +
        ggplot2::coord_cartesian(ylim = c(14, 19) 
        ) 
    }
  ) 

E + ggplot2::scale_x_reverse()

##NAmerica

N <- purrr::map(
  .x = namerica,
  .f = ~ {
    p +
      ggplot2::geom_ribbon(
        data = .x,
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
        data = .x,
        ggplot2::aes(x = age, y = richness),
        linewidth = 4, color = 'red'
      ) +
      ggplot2::theme(legend.position = "none",
                     axis.text  = element_text(size = 22),
                     strip.text = element_text(
                       size = 16,
                       color = "#2a707f"
                     ),
                     strip.background = element_rect(
                       color = "#2a707f",
                       fill = NA,
                       linewidth = 0.3
                     )
      ) +
      ggplot2::coord_cartesian(ylim = c(14, 19) 
      ) 
  }
) 

N + ggplot2::scale_x_reverse()

##combine continental trends into single plot

library(patchwork)

A + E + N
