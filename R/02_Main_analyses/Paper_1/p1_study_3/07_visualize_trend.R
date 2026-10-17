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

##2.0. Load back transformed richness datasets

data_back <- 
  list.files(
    "Data/Paper_1/data_model/data_back_2",
    pattern = "[.]rds$",
    full.names = TRUE)


## 2.1. Batch loading of back-transformed richness dataset (1000)

### 2.1.1. Batches of 50

batch_size <- 50

file_groups <- 
  split(1:1000, ceiling(1:1000 / batch_size))

### 2.1.2. Initialize an empty list to collect batches

all_batches <- list()

### 2.1.3. Process each batch

for (i in seq_along(file_groups)) {
  
  all_batches[[i]] <-
    file_groups[[i]] %>%
    map(load_pred_richness_and_select) %>%
    list_rbind()
  
  gc()
}

### 2.1.4. Combine all batches

combined_data <-
  list_rbind(all_batches)


rm(all_batches)
gc()

### 2.1.5. Load all batches

median_richness_back <- 
  combined_data %>%
  group_by(region, age) %>%
  summarise(
    continental_median_richness = median(richness, na.rm = TRUE),
    continental_richness_upp = quantile(richness, 0.975, na.rm = TRUE),
    continental_richness_dwn = quantile(richness, 0.025, na.rm = TRUE),
    .groups = "drop"
  )

rm(combined_data)
gc()


write_csv(median_richness_back,here("Data/Paper_1/data_model/model_csvs/S3_Preds.csv"))


median_richness_back <- read_csv(here("Data/Paper_1/data_model/model_csvs/S3_Preds.csv"))


##2.2. Load richness datasets

data_rich <-
  list.files(
  "Data/Paper_1/data_estimate_richness/richness_iters",
data_rich <- list.files(
  "Data/Paper_1/data_estimate_richness/s3_richness_test",
  pattern = "[.]rds$",
  full.names = TRUE
)

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
)

median_richness_back <- 
  purrr::map(1:1000, 
           .progress = TRUE,
           .f = load_pred_richness_and_select) %>% 
  bind_rows(.id = "iter") %>% 
  group_by(region, age) %>% 
  summarise (
  (
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
  geom_line(linewidth = 2, color = "red") + 
  geom_line(linewidth = 1, color = "red") + 
  geom_line(aes(x = age, y = continental_median_richness, group = iter),
    linewidth = 0.1, 
    alpha = 0.05, 
    color = "gray60"
  ) +

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
    


#Europe


eu_s3 <- median_richness_back %>% 
  filter(region == "Europe") %>% 
  rename(est= continental_median_richness,
         low = continental_richness_dwn,
         upp = continental_richness_upp)

gen_trend_eu_s3 <- 
  plot_trend_general(eu_s3 , "darkorchid3") 

gen_trend_eu_s3  + coord_cartesian(ylim = c(15,36))



# Asia

as_s3 <- 
  median_richness_back %>% 
  filter(region == "Asia") %>% 
  rename(est= continental_median_richness,
         low = continental_richness_dwn,
         upp = continental_richness_upp)

gen_trend_as_s3 <- 
  plot_trend_general(as_s3 , "red") 

gen_trend_as_s3  + coord_cartesian(ylim = c(15,23))

# N. America


na_s3 <- median_richness_back %>% 
median_richness_back %>% 
  filter(region == "North America") %>% 
  rename(est= continental_median_richness,
         low = continental_richness_dwn,
         upp = continental_richness_upp)

gen_trend_as_s3 <- 
  plot_trend_general(na_s3 , "orange") 

gen_trend_as_s3  + coord_cartesian(ylim = c(12,23))


#----------------------------------------------------------#
# 4. Visualize trends per iteration with site-level trends --
#----------------------------------------------------------# 

##4.1. for single iteration

readr::read_rds(data_rich[[1]]) %>% summary()

### e.g. for iteration 1

plot_richness_iter(data_back,data_rich,1)

##4.2. for all iterations and summarized to one trend line
##2.2. load all iterations and summarized to one trend line

all_data_back_iters <- purrr::map(
  .progress = TRUE,
  .x  = data_back,
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
    linewidth = 0.1, alpha = 0.05, color = "gray60") +
  geom_ribbon(
    data = all_data_back_iters_overall_AS,
    aes(x = age, ymin = lwr, ymax = upr),
    fill = "blue", alpha = 0.3) +
  geom_line(
    data = all_data_back_iters_overall_AS,
    aes(x = age, y = overall_median),
    color = "red", linewidth = 1) +
  scale_x_reverse() +
  coord_cartesian(ylim = c(15, 20)) +
  labs(x = "Age (cal yr BP)", y = "Richness") +
  theme_classic() 



#Europe

all_data_back_iters_new_EU <-
  all_data_back_iters_new %>% filter(region =="Europe")

all_data_back_iters_overall_EU <-
  all_data_back_iters_overall %>% filter(region =="Europe") 

ggplot() +
  geom_line(
    data = all_data_back_iters_new_EU,
    aes(x = age, y = median_richness, group = iter),
    linewidth = 0.1, alpha = 0.05, color = "gray60") +
  geom_ribbon(
    data = all_data_back_iters_overall_EU,
    aes(x = age, ymin = lwr, ymax = upr),
    fill = "blue", alpha = 0.3) +
  geom_line(
    data = all_data_back_iters_overall_EU,
    aes(x = age, y = overall_median),
    color = "red", linewidth = 1) +
  scale_x_reverse() +
  coord_cartesian(ylim = c(15, 25)) +
  labs(x = "Age (cal yr BP)", y = "Richness") +
  theme_classic() 


#NAmerica

all_data_back_iters_new_NA <-
  all_data_back_iters_new %>% filter(region =="North America")

all_data_back_iters_overall_NA <-
  all_data_back_iters_overall %>% filter(region =="North America") 

ggplot() +
  geom_line(
    data = all_data_back_iters_new_NA,
    aes(x = age, y = median_richness, group = iter),
    linewidth = 0.1, alpha = 0.05, color = "gray60") +
  geom_ribbon(
    data = all_data_back_iters_overall_NA,
    aes(x = age, ymin = lwr, ymax = upr),
    fill = "blue", alpha = 0.3) +
  geom_line(
    data = all_data_back_iters_overall_NA,
    aes(x = age, y = overall_median),
    color = "red", linewidth = 1) +
  scale_x_reverse() +
  coord_cartesian(ylim = c(15, 18)) +
  labs(x = "Age (cal yr BP)", y = "Richness") +
  theme_classic() 
       mutate(median_richness = median(richness),
              ci_lower = quantile(richness, 0.25, na.rm = TRUE),
              ci_upper = quantile(richness, 0.975, na.rm = TRUE) 
     
       )
  }
)
