# Load up dataset & preliminary data processing

library(tidyverse)
library(here)

data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

N_hemisphere <- data %>% 
  filter(region %in% c("North America", "Europe", "Asia")) %>% 
  relocate(region)


# Estimate richness per each sample within each record (core/dataset) within Northern hemisphere and plot it temporal trends per continent (region).

data %>%
     group_by(region,dataset_id) %>% 
     unnest(n_sample_counts) %>% 
     unnest(counts_harmonised) %>% 
     mutate(avg_age = mean (age_uncertainty) %>% 
     ggplot(aes( x = avg_age, y = n_sample_counts))) +
     geom_bar()

# 1

N_hemisphere %>% 
  group_by(dataset_id, region, n_sample_counts) %>% 
  mutate(avg_sample_counts = mean(n_sample_counts),
            age_range = age_max - age_min) %>% 
  ggplot(aes(y = avg_sample_counts , x = age_range, color = region)) +
  geom_line() +
  geom_smooth(method = "loess") +
  theme_classic()

# 2

N_hemisphere %>% 
  group_by(dataset_id, region, n_sample_counts) %>% 
  mutate(avg_sample_counts = mean(n_sample_counts),
         age_range = age_max - age_min) %>% 
            ggplot(aes(y = avg_sample_counts , x = age_range, color = region)) +
     geom_line() +
  geom_smooth(method = "loess") +
  theme_classic()

# Binning - sum pollen count per each taxa (across samples) within a specific time period. Make a function to do this 

data %>% 
  group_by(dataset_id) %>% 
  unnest(n_sample_counts)

N_hemisphere %>% 
  select(n_sample_counts, raw_counts) %>% 
  unnest(raw_counts)

view <- data %>% 
  select(n_sample_counts, raw_counts) %>% 
  unnest(raw_counts)

view

hemis <- N_hemisphere %>% 
  mutate(age_range = age_max - age_min)

hemis %>% 
  relocate(age_range) %>% 
  arrange(desc(age_range))



pollen_sum_taxa <- function(df,age,taxa, each, bins = 500) {
  df %>% 
      filter(age) %>% 
    ggplot(aes(x = {{taxa}}, y = {{sum(each)}})) +
    geom_bar(aes(bins = bins))
}

pollen_sum_taxa <- function(df, age, taxa, each, bins = 500) {
  df %>% 
    filter(age) %>% 
    ggplot(aes(x {{taxa}}, y ={{sum(each)}})) +
    geom_bar(aes(bins = bins))
}

geom_bar()
hemis %>% pollen_sum_taxa(age_range == 19000, sample_id, raw_counts, bins = 500)


hemis %>% 
  unnest(raw_counts)


# start here

 look <- N_hemisphere %>% 
  unnest(raw_counts) %>% 
    pivot_longer(
      cols = abies:betula_sect_costatae,
       names_to = "taxa",
          values_to = "count") %>% 
            relocate(taxa, count) 
 look               
                   

 pollen_sum_taxa <- function(df,age_range, x, y) {
     df %>% 
     mutate(age_range = age_max - age_min) %>% 
     filter(age_range <= 5000) %>%
     group_by(dataset_id, sample_id, taxa) %>% 
     mutate(total = sum(count)) %>% 
     ggplot(aes (x = (x), y = fct_reorder(x, y))) +
     geom_col()
   }

 data_1 %>% pollen_sum_taxa(age_range,total,taxa)
 
 
 data_1 %>% 
   mutate(age_range = age_max - age_min) %>% 
      filter(age_range == 5000) %>%
          group_by(dataset_id, sample_id)
               summarize(total = sum(count))
            ggplot(aes (x = taxa, y = age_range)) +
                    geom_histogram(bins = bins)
            
            
# this is OK   
            
data_1 %>% 
         mutate(age_range = age_max - age_min) %>% 
         filter(age_range <= 5000) %>%
         group_by(dataset_id, sample_id, taxa) %>% 
         summarize(total = sum(count)) %>% 
         ggplot(aes (x = (total), y = fct_reorder(taxa, total))) +
         geom_col()

pollen_sum_taxa <- function(df,time_period, total, taxa) {
  df %>% 
    mutate(age_range = age_max - age_min) %>% 
    filter({{time_period}}) %>%
    group_by({{dataset_id, sample_id, taxa}}) %>% 
    summarize({{total = sum(count)}} %>% 
    ggplot(aes (x = ({{total}}), y = fct_reorder({{taxa,total}}))) +
    geom_col()
}

data_1 %>%
  pollen_sum_taxa(time_period == 5000,total,taxa)


geom()
# Estimate richness per each 500  year bin within each record (core/dataset) within Northen hemispehe and plot it temporal trends per continent (region). 

N_hemisphere

data_1 <- N_hemisphere %>% 
  unnest(raw_counts) %>% 
  pivot_longer(
    cols = abies:betula_sect_costatae,
      names_to = "taxa",
       values_to = "count", 
         values_drop_na = TRUE) %>% 
           mutate(time_period = age_max - age_min) %>% 
              relocate(taxa, count, time_period) 


pollen_sum_taxa <- function(df,condition,var_1, var_2){
  df %>% 
    filter({{condition}}) %>% 
    summarize(sum({{var_1}})) %>% 
    ggplot(aes(x = {{var_2}}, y = {{var_1}})) +
    geom_col()
}

data_1
data_2
data_3


data_1 <- pollen_sum_taxa(time_period == 5000, count, taxa)

data_1
View(data_1)

quit()
