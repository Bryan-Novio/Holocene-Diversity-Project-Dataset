#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                       Novio & Mottl
#
#                          2026
#
# 
#
#  ----  Compute stats from Digitized Figures (Study 1 - 4) ----
#----------------------------------------------------------#

## Load libraries

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load csv files -----------------------------------------
#----------------------------------------------------------# 

# Data from original study

s1 <- read_csv(here("Data/Paper_1/data_digitise/s1.csv"))
s2 <- read_csv(here("Data/Paper_1/data_digitise/s2.csv"))
s3_eu <- read_csv(here("Data/Paper_1/data_digitise/s3_eu.csv"))
s3_na <- read_csv(here("Data/Paper_1/data_digitise/s3_na.csv"))
s3_as <- read_csv(here("Data/Paper_1/data_digitise/s3_as.csv"))
s4 <- read_csv(here("Data/Paper_1/data_digitise/s4.csv"))

# Reformat data- original

s1_new <- s1 %>% 
  rename(age = x, richness = y, site = id) %>% 
  select(site, age, richness, group) %>% 
  mutate(site = as_factor(site)) %>% 
  mutate(site, fct_recode(site, "Alps" = "col",
                                "Boreal" = "green",
                                "Temperate Oceanic" = "blue",
                                "Meridional/Submeridional"= "red",
                                "Temperate Continental"= "orange")) %>% 
  select(-c(site, group)) %>% 
  rename(site = `fct_recode(...)`) %>% 
  relocate(site)

s2_new <- s2 %>% 
  rename(time = x, diversity = y) %>% 
  select(id, time, diversity) %>% 
  rename(estimate = diversity, age = time) %>% 
  mutate(age = round(age)) %>% 
  pivot_wider(names_from = id, values_from = "estimate") %>% 
  mutate(age = age *1000)

s3_eu_new <- s3_eu %>% 
  rename(age = x, richness = y) %>% 
  select(id, age, richness) %>% 
  mutate(age = round(age, -3)) %>% 
  pivot_wider(names_from = id, values_from = "richness")

s3_na_new <- s3_na %>% 
  rename(age = x, richness = y) %>% 
  select(id, age, richness) %>% 
  mutate(age = round(age, -3)) %>% 
  pivot_wider(names_from = id, values_from = "richness")


s3_as_new <- s3_as %>% 
  rename(age = x, richness = y) %>% 
  select(id, age, richness) %>% 
  mutate(age = round(age, -3)) %>% 
  distinct() %>%  # remove duplicates
  filter(!row_number()%in% c(9)) %>% # remove more duplicates 
  pivot_wider(names_from = id, values_from = "richness")


s4_new <- s4 %>% 
  rename(age = x, richness = y) %>% 
  select(id,age, richness) %>% 
  rename(estimate = richness) %>% 
  mutate(age = round(age)) %>% 
  mutate(age = age*1000) %>% 
  pivot_wider(names_from = id, values_from = "estimate")

# Data from Replication

s1_rep <- 
  read_csv(here("Data/Paper_1/data_model/model_csvs/S1_Richness.csv")) %>% 
  rename(richness = median_richness,
         site = subregion) %>%
  relocate(site)

s2_rep <- 
  read_csv(here("Data/Paper_1/data_model/model_csvs/S2_Preds.csv")) %>% 
  select(estimate, age, conf_high, conf_low)

s3_rep <- 
  read_csv(here("Data/Paper_1/data_model/model_csvs/S3_Preds.csv"))

s4_rep <- 
  read_csv(here("Data/Paper_1/data_model/model_csvs/S4_Preds.csv"))

#----------------------------------------------------------#
# 2. Check if point estimate in rep is within CIs of original -----------------------------------------
#----------------------------------------------------------# 

#study 2

# round to neareas thousands

s2_rep_round <- 
  s2_rep %>%
  mutate(age = round(age, -3)) 

s2_bind <- s2_new %>%
  left_join(s2_rep_round , by = "age") %>% 
  select(age,estimate,upp,low) %>% 
  rename(richness = estimate) 

labs <-  c(0,5,10,15,20)

s2_bind %>% 
  ggplot(aes(x = age, y = estimate)) +
  geom_point(size = 4, colour = "red", shape = 4) +
  geom_ribbon(aes(ymin = low, ymax = upp), fill = "blue", alpha = 0.3) +
  labs(y = "Estimate", x = "cal yr BP") +
  theme(axis.title.x = element_text(size = 20),
         axis.title.y = element_text(size = 20),
         axis.text.x = element_text(size = 20),
         axis.text.y  = element_text(size = 20),
        panel.background = element_blank(),
        panel.border =  element_rect(colour = "black")
         ) +
  scale_x_reverse(labels = labs) 

# study 3

##Europe

s3_eu_rep_new <- s3_rep %>% 
  filter (region == "Europe") %>% 
  mutate(age = round(age, -3)) %>% 
  rename(richness = continental_median_richness,
         upp = continental_richness_upp,
         low = continental_richness_dwn) %>% 
  select(age, richness)
  

labs_eu <-  c(0,2.5,5,7.5,10,12.5)

s3_eu_bind <- 
  left_join(s3_eu_new, s3_eu_rep_new, by = "age") %>% 
  select(age, richness, upp, low)

s3_eu_bind %>% 
  ggplot(aes(x = age, y = richness)) +
  geom_point(size = 4, colour = "red", shape = 4) +
  geom_ribbon(aes(ymin = low, ymax = upp), fill = "blue", alpha = 0.3) +
  labs(y = "Estimate", x = "cal yr BP") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y  = element_text(size = 16),
        panel.background = element_blank(),
        panel.border =  element_rect(colour = "black")
  ) +
  scale_x_reverse(labels = labs_eu) 


## NAmerica

s3_na_new 

s3_na_rep_new <- s3_rep %>% 
  filter (region == "North America") %>% 
  mutate(age = round(age, -3)) %>% 
  rename(richness = continental_median_richness,
         upp = continental_richness_upp,
         low = continental_richness_dwn) %>% 
  select(age, richness)

s3_na_bind <- 
  left_join(s3_na_new, s3_na_rep_new, by = "age") %>% 
  select(age, richness, upp, low)


s3_na_bind %>% 
  ggplot(aes(x = age, y = richness)) +
  geom_point(size = 4, colour = "red", shape = 4) +
  geom_ribbon(aes(ymin = low, ymax = upp), fill = "blue", alpha = 0.3) +
  labs(y = "Estimate", x = "cal yr BP") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y  = element_text(size = 16),
        panel.background = element_blank(),
        panel.border =  element_rect(colour = "black")
  ) +
  scale_x_reverse(labels = labs_eu) 


# Asia

s3_as_rep_new <- s3_rep %>% 
  filter (region == "Asia") %>% 
  mutate(age = round(age, -3)) %>% 
  rename(richness = continental_median_richness,
         upp = continental_richness_upp,
         low = continental_richness_dwn) %>% 
  select(age, richness)

s3_as_bind <- 
  left_join(s3_as_new , s3_as_rep_new, by = "age") %>% 
  select(age, richness, upp, low)

s3_as_bind %>% 
  ggplot(aes(x = age, y = richness)) +
  geom_point(size = 4, colour = "red", shape = 4) +
  geom_ribbon(aes(ymin = low, ymax = upp), fill = "blue", alpha = 0.3) +
  labs(y = "Estimate", x = "cal yr BP") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y  = element_text(size = 16),
        panel.background = element_blank(),
        panel.border =  element_rect(colour = "black")
  ) +
  scale_x_reverse() 


# Study 4


s4_rep_round <- 
  s4_rep %>%
  mutate(age = round(age)) %>% 
  mutate(age = round(age, -3))

s4_bind <- s4_new %>%
  left_join(s4_rep_round , by = "age") %>% 
  select(age,estimate,upp,low) %>% 
  drop_na() %>% 
  rename(richness = estimate) 


s4_bind %>% 
  ggplot(aes(x = age, y = estimate)) +
  geom_point(size = 4, colour = "red", shape = 4) +
  geom_ribbon(aes(ymin = low, ymax = upp), fill = "blue", alpha = 0.3) +
  labs(y = "Estimate", x = "cal yr BP") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y  = element_text(size = 20),
        panel.background = element_blank(),
        panel.border =  element_rect(colour = "black")
  ) +
  scale_x_reverse(labels = labs_eu) 


all_studies <- 
  bind_rows(s2_bind,s3_eu_bind,s3_na_bind,s3_as_bind,s4_bind, .id = "id") %>% 
  mutate(study = id) %>% 
  mutate(study, fct_recode(study, "Study 2" = "1",
                          "Study 3_EU" = "2",
                          "Study 3_NA" = "3",
                          "Study 3_AS"= "4",
                          "Study 4"= "5")) %>% 
  select(-id, study) %>% 
  mutate(study = `fct_recode(...)`) %>% 
  select(-`fct_recode(...)`) %>% 
  relocate(study)

#Combined plot

ci_pt <- all_studies %>% 
  ggplot(aes(x = age, y = richness)) +
  geom_point(size = 4, colour = "red", shape = 4) +
  geom_ribbon(aes(ymin = low, ymax = upp), fill = "blue", alpha = 0.3) +
  labs(y = "Estimate", x = "cal yr BP") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y  = element_text(size = 20),
        panel.background = element_blank(),
        panel.border =  element_rect(colour = "black")
  ) +
  scale_x_reverse() 

ci_pt + facet_grid(cols = vars(study))










