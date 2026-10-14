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
# 1. Set -up -----------------------------------------
#----------------------------------------------------------# 
## 1.1. Load functions

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
# 2. Load datasets -----------------------------------------
#----------------------------------------------------------# 

data_rich <- list.files(
  "Data/Paper_1/data_estimate_richness/s3_richness_test",
  pattern = "[.]rds$",
  full.names = TRUE
) %>% 
  rlang::set_names(
    nm = str_extract(., "\\d+(?=\\.rds$)")
  )


data_rich[1:10]

data_back <- list.files(
  "Data/Paper_1/data_model/data_back_new/",
  pattern = "[.]rds$",
  full.names = TRUE
) %>% 
  rlang::set_names(
    nm = str_extract(., "\\d+(?=\\.rds$)")
  )

data_back[1:10]

data_back[["1"]] %>% read_rds()


median_richness_back <-
  1:1000 %>% 
  rlang::set_names() %>% 
  purrr::map(
           .progress = TRUE,
           .f = load_pred_richness_and_select) %>% 
  bind_rows(.id = "iter") %>% 
  group_by(region, age) %>% 
  summarise (
    continental_median_richness = median(richness),
    continental_richness_upp = quantile(richness, 0.975),
    continental_richness_dwn = quantile(richness, 0.025)
  )


#----------------------------------------------------------#
# 3. Visualise continental trends -------------------------
#----------------------------------------------------------# 

# All continents in one plot

median_richness_back %>% 
  ggplot(aes(x = age, y = continental_median_richness)) +
  geom_line(linewidth = 1, color = "red") +  
  geom_ribbon(aes(ymin = continental_richness_dwn, 
  ymax = continental_richness_upp),  fill = "blue", alpha = 0.4) +
  facet_wrap(~region) +
  theme_classic()+
  scale_x_reverse()

#----------------------------------------------------------#
# 4. Visualize trends per iteration with site-level trends --
#----------------------------------------------------------# 


##4.1. for single iteration

readr::read_rds(data_rich[[1]]) %>% summary()

### e.g. for iteration 1

plot_richness_iter(data_back,data_rich,1)



data_rich_AS <- 
  data_rich[["1"]] %>%
  read_rds() %>% 
  filter(region =="Asia")

data_back[["1"]] %>% 
  readr::read_rds() %>% 
  filter(region =="Asia") %>% 
  ggplot(aes(x = age, y = richness)) +
  geom_line(data = data_rich_AS, aes(group = dataset_id), linewidth = 0.1, alpha = 0.1, color ="coral") +
  geom_line(linewidth = 2, color= "darkred") +
  geom_ribbon(aes(ymin = rich_low, ymax = rich_high, y = NULL), fill = "red", alpha = 0.3) +
  labs(x = "Age (cal yr BP)", y = "Richness") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.background = element_blank(),
        axis.line.x = element_line(linewidth = 0.5),
        axis.line.y = element_line(linewidth = 0.5),
        axis.ticks.x.bottom = element_line(linewidth = 1),
        axis.ticks.y.left = element_line(linewidth = 1)
  ) +
  scale_x_reverse()



data_rich_NA <- 
  data_rich[["1"]] %>%
  read_rds() %>% 
  filter(region =="North America")


data_back[["1"]] %>% 
  readr::read_rds() %>% 
  filter(region =="North America") %>% 
  ggplot(aes(x = age, y = richness)) +
  geom_line(data = data_rich_NA, aes(group = dataset_id), linewidth = 0.1, alpha = 0.04, colour = "orange") +
  geom_line(linewidth = 2, color= "darkorange4", alpha = 2) +
  geom_ribbon(aes(ymin = rich_low, ymax = rich_high, y = NULL), fill = "darkorange4", alpha = 0.3) +
  labs(x = "Age (cal yr BP)", y = "Richness") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.background = element_blank(),
        axis.line.x = element_line(linewidth = 0.5),
        axis.line.y = element_line(linewidth = 0.5),
        axis.ticks.x.bottom = element_line(linewidth = 1),
        axis.ticks.y.left = element_line(linewidth = 1)
  ) +
  scale_x_reverse()

data_rich_EU <- 
  data_rich[["1"]] %>%
  read_rds() %>% 
  filter(region =="Europe")

data_back[["1"]] %>% 
  readr::read_rds() %>% 
  filter(region =="Europe") %>% 
  ggplot(aes(x = age, y = richness)) +
  geom_line(data = data_rich_EU, aes(group = dataset_id), linewidth = 0.1, alpha = 0.1, color ="violet") +
  geom_line(linewidth = 2, color= "darkviolet") +
  geom_ribbon(aes(ymin = rich_low, ymax = rich_high, y = NULL), fill = "darkviolet", alpha = 0.6) +
  labs(x = "Age (cal yr BP)", y = "Richness") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.background = element_blank(),
        axis.line.x = element_line(linewidth = 0.5),
        axis.line.y = element_line(linewidth = 0.5),
        axis.ticks.x.bottom = element_line(linewidth = 1),
        axis.ticks.y.left = element_line(linewidth = 1)
  ) +
  scale_x_reverse()



data_rich_NA <- 
  data_rich[["1"]] %>%
  read_rds() %>% 
  filter(region =="North America")


data_back[["1"]] %>% 
  readr::read_rds() %>% 
  filter(region =="North America") %>% 
  ggplot(aes(x = age, y = richness)) +
  geom_line(data = data_rich_NA, aes(group = dataset_id), linewidth = 0.1, alpha = 0.04, colour = "orange") +
  geom_line(linewidth = 2, color= "orange", alpha = 2) +
  geom_ribbon(aes(ymin = rich_low, ymax = rich_high, y = NULL), fill = "darkgoldenrod1", alpha = 0.3) +
  labs(x = "Age (cal yr BP)", y = "Richness") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.background = element_blank(),
        axis.line.x = element_line(linewidth = 0.5),
        axis.line.y = element_line(linewidth = 0.5),
        axis.ticks.x.bottom = element_line(linewidth = 1),
        axis.ticks.y.left = element_line(linewidth = 1)
  ) +
  scale_x_reverse()



##4.2. for all iterations and summarized to one trend line

all_data_back_iters <- 
  nm <- 1:1000 %>% 
  rlang::set_names() %>%
  purrr::map(
  .x = data_back,
  .progress = TRUE,
  .f  =  ~ {
    
    iters <- .x %>% 
      readr::read_rds()
    
     iters_new <- iters %>% 
       group_by(region,age) %>% 
       summarise(median_richness = median(richness),.groups = "drop",
              ci_lower = quantile(richness, 0.025, na.rm = TRUE),
              ci_upper = quantile(richness, 0.975, na.rm = TRUE) 
     
       )
     return(iters_new)
  }
)

##4.3. combine all iterations as a dataframe

all_data_back_iters_new <-
  bind_rows(all_data_back_iters , .id = "iter")



## 4.4. collapse iterations into one summary curve per (region, age)

all_data_back_iters_overall <- all_data_back_iters_new %>%
  group_by(region, age) %>%
  summarize(
    overall_median = median(median_richness, na.rm = TRUE),
    lwr = quantile(median_richness, 0.025, na.rm = TRUE),
    upr = quantile(median_richness, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

## 4.5. Plot median richness per iter with overall median and error

ggplot() +
  geom_line(
    data = all_data_back_iters_new,
    aes(x = age, y = median_richness, group = iter),
    linewidth = 0.1, alpha = 0.05, color = "gray60") +
  geom_ribbon(
    data = all_data_back_iters_overall,
    aes(x = age, ymin = lwr, ymax = upr),
    fill = "blue", alpha = 0.3) +
  geom_line(
    data = all_data_back_iters_overall,
    aes(x = age, y = overall_median),
    color = "red", linewidth = 0.5) +
  facet_wrap(~ region) +
  scale_x_reverse() +
  coord_cartesian(ylim = c(15, 30)) +
  labs(x = "Age (cal yr BP)", y = "Richness") +
  theme_classic() 

#Asia

all_data_back_iters_new_AS <-
  all_data_back_iters_new %>% filter(region =="Asia")

all_data_back_iters_overall_AS <-
  all_data_back_iters_overall %>% filter(region =="Asia") 

ggplot() +
  geom_line(
    data = all_data_back_iters_new_AS,
    aes(x = age, y = median_richness, group = iter),
    linewidth = 0.1, alpha = 0.05, color = "red") +
  geom_ribbon(
    data = all_data_back_iters_overall_AS,
    aes(x = age, ymin = lwr, ymax = upr),
    fill = "coral", alpha = 0.3) +
  geom_line(
    data = all_data_back_iters_overall_AS,
    aes(x = age, y = overall_median),
    color = "darkred", linewidth = 1) +
  scale_x_reverse() +
  coord_cartesian(ylim = c(15, 18.5)) +
  labs(x = "Age (cal yr BP)", y = "Richness") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.background = element_blank(),
        axis.line.x = element_line(linewidth = 0.5),
        axis.line.y = element_line(linewidth = 0.5),
        axis.ticks.x.bottom = element_line(linewidth = 1),
        axis.ticks.y.left = element_line(linewidth = 1)
  )



#Europe

all_data_back_iters_new_EU <-
  all_data_back_iters_new %>% filter(region =="Europe")

all_data_back_iters_overall_EU <-
  all_data_back_iters_overall %>% filter(region =="Europe") 

ggplot() +
  geom_line(
    data = all_data_back_iters_new_EU,
    aes(x = age, y = median_richness, group = iter),
    linewidth = 0.1, alpha = 0.05, color = "violet") +
  geom_ribbon(
    data = all_data_back_iters_overall_EU,
    aes(x = age, ymin = lwr, ymax = upr),
    fill = "darkviolet", alpha = 0.3) +
  geom_line(
    data = all_data_back_iters_overall_EU,
    aes(x = age, y = overall_median),
    color = "darkviolet", linewidth = 1) +
  scale_x_reverse() +
  coord_cartesian(ylim = c(15, 24)) +
  labs(x = "Age (cal yr BP)", y = "Richness") +
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        panel.background = element_blank(),
        axis.line.x = element_line(linewidth = 0.5),
        axis.line.y = element_line(linewidth = 0.5),
        axis.ticks.x.bottom = element_line(linewidth = 1),
        axis.ticks.y.left = element_line(linewidth = 1)
  )


#NAmerica

all_data_back_iters_new_NA <-
  all_data_back_iters_new %>% filter(region =="North America")

all_data_back_iters_overall_NA <-
  all_data_back_iters_overall %>% filter(region =="North America") 

ggplot() +
  geom_line(
    data = all_data_back_iters_new_NA,
    aes(x = age, y = median_richness, group = iter),
    linewidth = 0.1, alpha = 0.05, color = "orange") +
  geom_ribbon(
    data = all_data_back_iters_overall_NA,
    aes(x = age, ymin = lwr, ymax = upr),
    fill = "darkorange", alpha = 0.3) +
  geom_line(
    data = all_data_back_iters_overall_NA,
    aes(x = age, y = overall_median),
    color = "darkorange", linewidth = 1) +
  scale_x_reverse() +
  coord_cartesian(ylim = c(15, 17.7
                           )) +
  labs(x = "Age (cal yr BP)", y = "Richness") + 
  theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            panel.background = element_blank(),
            axis.line.x = element_line(linewidth = 0.5),
            axis.line.y = element_line(linewidth = 0.5),
            axis.ticks.x.bottom = element_line(linewidth = 1),
            axis.ticks.y.left = element_line(linewidth = 1)
  )


