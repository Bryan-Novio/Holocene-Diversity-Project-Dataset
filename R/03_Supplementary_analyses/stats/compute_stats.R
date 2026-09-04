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
# 1. Load csv files ----------------------------------
#----------------------------------------------------------# 

# Data from Replicated Studies


s1 <- read_csv(here("Data/Paper_1/data_digitise/s1.csv"))
s2 <- read_csv(here("Data/Paper_1/data_digitise/s2.csv"))
s3_eu <- read_csv(here("Data/Paper_1/data_digitise/s3_eu.csv"))
s3_na <- read_csv(here("Data/Paper_1/data_digitise/s3_na.csv"))
s3_as <- read_csv(here("Data/Paper_1/data_digitise/s3_as.csv"))
s4 <- read_csv(here("Data/Paper_1/data_digitise/s4.csv"))

# Data from Replication

s1_rep <- read_csv(here("Data/Paper_1/data_model/model_csvs/S1_Richness.csv"))
s2_rep <- read_csv(here("Data/Paper_1/data_model/model_csvs/S2_Preds.csv")) 
s3_rep <- read_csv(here("Data/Paper_1/data_model/model_csvs/S3_Preds.csv"))
s4_rep <- read_csv(here("Data/Paper_1/data_model/model_csvs/S4_Preds.csv"))

#----------------------------------------------------------#
# 2. Reformat data files from each study ---------------
#----------------------------------------------------------# 

# 2.1. Data from Replicated Studies

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


# Plot S1

s1_new %>% 
  ggplot(aes(x = age, y = richness, color = site))  +
  annotate("rect", xmin = Inf, xmax = 11500, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.2) +
  annotate("rect", xmin = 11500, xmax = 8500, ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.2) +
  annotate("rect", xmin = 8500, xmax = 4500, ymin = -Inf, ymax = Inf, fill = "lightyellow", alpha = 0.2) +
  annotate("rect", xmin = 4500, xmax = 0, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.2) +
  geom_line(linewidth = 1.5) +
  scale_x_reverse(
    breaks = seq(0, 15000, by = 1000),
    labels = function(x) {
      if_else(x %in% c(0, 3000, 6000, 9000, 12000, 15000), as.character(x), "")
    }
  ) +
  scale_color_manual(values = c(
    "Alps" = "black",
    "Boreal" = "darkgreen",
    "Meridional/Submeridional" = "red",
    "Temperate Continental" = "orange",
    "Temperate Oceanic" = "blue"
  )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
         theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y  = element_text(size = 20),
        panel.background = element_blank(),
        panel.border =  element_rect(colour = "black"))) +
  labs(y = expression(Median~site~richness~(ET[500]))) +
  labs(x = "Age in years ago")


# Plot S2



s2_new <- s2 %>% 
  rename(time = x, diversity = y) %>% 
  select(id, time, diversity) %>% 
  rename(estimate = diversity, age = time) %>% 
  mutate(age = round(age)) %>% 
  pivot_wider(names_from = id, values_from = "estimate") %>% 
  mutate(age = age *1000)

s2_new %>% 
ggplot2::ggplot(aes(x = age, y = est)) +
  ggplot2::labs(
    y = "Pollen Richness", x = "Age (cal yr BP)"
  )+ 
  ggplot2::geom_line(color = "black", linewidth = 1
  ) +
  geom_ribbon(aes(ymin = low, ymax = upp), colour = "gray",alpha = 0.5
  ) +
  ggplot2::theme_classic(
  ) +
  ggplot2::coord_cartesian(
    ylim = c(6,14)
  ) +
  ggplot2::scale_x_reverse() +
  ggplot2::geom_vline(xintercept = 9500, linetype = "dashed", color ="black")


#Plot S3-Asia

s3_as_new <- s3_as %>% 
  rename(age = x, richness = y) %>% 
  select(id, age, richness) %>% 
  mutate(age = round(age, -3)) %>% 
  distinct() %>%  # remove duplicates
  filter(!row_number()%in% c(9)) %>% # remove more duplicates 
  pivot_wider(names_from = id, values_from = "richness")


s3_as_new %>% 
  ggplot(aes(x = age, y = col)) +
  geom_line(linewidth = 4, color = "red") + 
  geom_ribbon(aes(ymin = low, 
                  ymax = upp),  fill = "red", alpha = 0.1) +
  labs(x = "Age(cal yr BP)" , y = "Richness") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15, hjust = 0.15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.line.y.left = element_line(color = "black", linewidth = 1),
        axis.line.x.bottom = element_line(color = "black", linewidth = 1)) +
  scale_x_reverse()


#Plot S3-Europe

s3_eu_new <- s3_eu %>% 
  rename(age = x, richness = y) %>% 
  select(id, age, richness) %>% 
  mutate(age = round(age, -3)) %>% 
  distinct(age, id, .keep_all = TRUE) %>%
  mutate(richness = num(richness, digits = 4)) %>% 
  pivot_wider(names_from = id, values_from = "richness") %>% 
  drop_na()



s3_eu_new %>% 
  ggplot(aes(x = age, y = est)) +
  geom_line(linewidth = 4, color = "purple") + 
  geom_ribbon(aes(ymin = low, 
                  ymax = upp),  fill = "purple", alpha = 0.1) +
  labs(x = "Age(cal yr BP)" , y = "Richness") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15, hjust = 0.15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.line.y.left = element_line(color = "black", linewidth = 1),
        axis.line.x.bottom = element_line(color = "black", linewidth = 1)) +
  scale_x_reverse()


#Plot S3-NAmerica

s3_na_new <- s3_na %>% 
  rename(age = x, richness = y) %>% 
  select(id, age, richness) %>% 
  mutate(age = round(age, -3)) %>% 
  pivot_wider(names_from = id, values_from = "richness")

s3_na_new %>% 
  ggplot(aes(x = age, y = est)) +
  geom_line(linewidth = 4, color = "orange") + 
  geom_ribbon(aes(ymin = low, 
                  ymax = upp),  fill = "orange", alpha = 0.1) +
  labs(x = "Age(cal yr BP)" , y = "Richness") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15, hjust = 0.15),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.line.y.left = element_line(color = "black", linewidth = 1),
        axis.line.x.bottom = element_line(color = "black", linewidth = 1)) +
  scale_x_reverse()


#Plot S4

s4_new <- s4 %>% 
  rename(age = x, richness = y) %>% 
  select(id,age, richness) %>% 
  rename(estimate = richness) %>% 
  mutate(age = round(age)) %>% 
  mutate(age = age*1000) %>% 
  pivot_wider(names_from = id, values_from = "estimate")



s4_new  %>% 
  ggplot(aes(x = age, y = est)) +
  geom_line(linewidth = 4, color = "black") + 
  geom_ribbon(aes(ymin = low, 
                  ymax = upp),  fill = "gray", alpha = 0.4) +
  labs(x = "Age(cal yr BP)" , y = "Richness") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15, hjust = 0.9),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),
        axis.line.y.left = element_line(color = "black", linewidth = 1),
        axis.line.x.bottom = element_line(color = "black", linewidth = 1)) +
  coord_cartesian(ylim = c(10.3,14.9))
  scale_x_reverse()





# 2.2. Data from Replication

s1_rep <- 
  read_csv(here("Data/Paper_1/data_model/model_csvs/S1_Richness.csv")) %>% 
  rename(richness = median_richness,
         site = subregion) %>%
  relocate(site)

s2_rep <- 
  read_csv(here("Data/Paper_1/data_model/model_csvs/S2_Preds.csv")) %>% 
  select(estimate, age, conf_high, conf_low)

s3_rep <- 
  read_csv(here("Data/Paper_1/data_model/model_csvs/S3_Preds.csv")) %>%    
  rename(richness = median_richness,
       site = subregion) %>%
  relocate(site)

s4_rep <- 
  read_csv(here("Data/Paper_1/data_model/model_csvs/S4_Preds.csv")) %>%    
  rename(richness = estimate) 

s4_rep %>% colnames()

#----------------------------------------------------------#
# 3. Re estimate Effect Sizes and CI ------------
#----------------------------------------------------------# 

# Check if point estimate in rep is within CIs of original

##study 2

s2_rep_round <- 
  s2_rep %>%
  mutate(age = round(age, -3)) 

s2_rep_round %>%
  group_by(age) %>%  # overall predicted : (9.8 - 12.61, CI 10.61 - 11.79) -replicated
  summary()

s2_rep_round_summ <- s2_rep_round %>%  
  group_by(age) %>% 
  summarise(estimate = mean(estimate),
            chi = mean(conf_high),
            clow = mean(conf_low))

s2_rep_round_summ_join <- 
  left_join(s2_new, s2_rep_round_summ, by = "age") # estimate + CI per time

s2_rep_round_summ_join %>%  summary()

# original (8.61 - 12.92, CI 10.59 - 11.33)

s2_bind <- s2_new %>%
  left_join(s2_rep_round , by = "age") %>% 
  select(age,estimate,upp,low) %>% 
  rename(richness = estimate) 

s2_bind_rep<- s2_rep_round %>% 
  group_by(age) %>% 
  summarise(mean_richness = mean(estimate),
           mean_upp = mean(conf_high),
           mean_low = mean(conf_low))

s2_bind_rep_1 <- s2_new %>%
  left_join(s2_bind_rep , by = "age") %>% 
  select(age, mean_richness, mean_upp, mean_low) %>% 
  rename(est = mean_richness, upp = mean_upp, low = mean_low)

labs <-  c(0,5,10,15,20)

s2_bind %>% 
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

s3_eu_rep_new_2 <- s3_rep %>% 
  filter (region == "Europe") %>% 
  mutate(age = round(age, -3)) %>% 
  rename(richness = continental_median_richness,
         conf_hi = continental_richness_upp,
         conf_low = continental_richness_dwn) %>% 
select(age, richness, conf_hi, conf_low) %>% 
  group_by(age) %>% 
  summarise(mean_richness = mean(richness),
            mean_conf_hi = mean(conf_hi),
            mean_low_hi = mean(conf_low))

s3_eu_rep_new_2 %>% summary() # replicated(22.59 - 36.50, CI 21.92 -37.02)

s3_eu_new %>%  summary() # original (16.12 - 26.30, CI 15.81 - 26.64)


s3_eu_rep_summ_join <- 
  left_join(s3_eu_new,s3_eu_rep_new_2, by = "age") # estimate + CI per time



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

s3_na_new %>% summary() # orig (15.64 - 16.50, CI 15.14 -16.88)


s3_na_rep_new <- s3_rep %>% 
  filter (region == "North America") %>% 
  mutate(age = round(age, -3)) %>% 
  rename(richness = continental_median_richness,
         upp = continental_richness_upp,
         low = continental_richness_dwn) %>% 
  select(age, richness)


s3_na_rep_new_2 <- s3_rep %>% 
  filter (region == "North America") %>% 
  mutate(age = round(age, -3)) %>% 
  rename(richness = continental_median_richness,
         conf_hi = continental_richness_upp,
         conf_low = continental_richness_dwn) %>% 
  select(age, richness,  conf_hi, conf_low) %>% 
  group_by(age) %>% 
  summarise(mean_richness = mean(richness),
            mean_conf_hi = mean(conf_hi),
            mean_low_hi = mean(conf_low))
  

s3_na_rep_new_2 %>% summary() #replicated (19.08 - 22.96, CI 18.89 - 23.25)

s3_na_rep_summ_join <- 
  left_join(s3_na_new,s3_na_rep_new_2, by = "age") # estimate + CI per time

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

s3_as_rep_new_2 <- s3_rep %>% 
  filter (region == "Asia") %>% 
  mutate(age = round(age, -3)) %>% 
  rename(richness = continental_median_richness,
         conf_high = continental_richness_upp,
         conf_low = continental_richness_dwn) %>% 
  select(age, richness, conf_high, conf_low) %>% 
  group_by(age) %>% 
  summarise(mean_richness =  mean(richness),
            mean_conf_high = mean(conf_high),
            mean_conf_low = mean(conf_low)
            )

s3_as_rep_new_2 %>% 
  summary()   # replicated (18.80 - 20.15, CI 17.95 - 20.78)


s3_as_new %>% summary() # original (16.49 - 17.48, CI 16.40 - 17.80)

s3_as_rep_round_summ_join <- 
  left_join(s3_as_new,s3_as_rep_new_2 , by = "age") # estimate + CI per time




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


s4_rep_round_summ <-
  s4_rep_round %>% 
  group_by(age) %>% 
  summarise(estimate = mean(estimate),
            chi = mean(conf_high),
            clow = mean(conf_low))

s4_rep_round_summ_join <- 
  left_join(s4_new, s4_rep_round_summ, by = "age") # estimate + CI per time


#plot join orig and rep


s4_rep_round_summ_join_sel <- s4_rep_round_summ_join %>% 
  select(age,estimate, chi,clow)
  



s4_new_re <- 
  s4_new %>% 
  rename(estimate = est, chi = upp, clow = low)


bind_s4 <- bind_rows(s4_new_re,
                     s4_rep_round_summ_join_sel, .id = "id") 

bind_s4 %>% 
  ggplot(aes(x = age, y = estimate, colour = id)) +
  geom_point(size = 6, shape = 19, linewidth = 2) +
  geom_line(linetype = 2)+
  geom_errorbar(aes(ymin = clow, ymax = chi), lineend = NULL, linewidth = 1,
                middle.linewidth = 0,  alpha = 0.5) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
  scale_x_reverse()

s4_rep_round_summ_join %>% summary() 
#original (12.20 - 12.67, CI 11.69 - 14.89)

s4_rep_round %>% summary() # replicated (14.06 - 16.50, CI 12.77 - 18.16)



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


#----------------------------------------------------------#
#4. Plotting Estimate + CIs for orig. & rep. studies-------
#----------------------------------------------------------#

# 4.1. Plot all estimates

ci_pt <- all_studies %>% 
  ggplot(aes(x = age, y = richness)) +
  geom_point(size = 5, colour = "red", shape = 4) +
  geom_ribbon(aes(ymin = low, ymax = upp), fill = "blue", alpha = 0.3) +
  labs(y = "Estimate (Rarefied Richness)", x = "cal yr BP") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y  = element_text(size = 20),
        panel.background = element_blank(),
        panel.border =  element_rect(colour = "black"),
        legend.position = "bottom") +
  scale_x_reverse() 


ci_pt + facet_grid(rows = vars(study))


#4.22 Plot combined studies (Estimates + CI)

ci_pt <- all_studies %>% 
  ggplot(aes(x = age, y = richness)) +
  geom_point(size = 5, colour = "red", shape = 4) +
  geom_ribbon(aes(ymin = low, ymax = upp), fill = "blue", alpha = 0.3) +
  labs(y = "Estimate (Rarefied Richness)", x = "cal yr BP") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y  = element_text(size = 20),
        panel.background = element_blank(),
        panel.border =  element_rect(colour = "black"),
        legend.position = "bottom") +
  scale_x_reverse() 


ci_pt + facet_grid(rows = vars(study))


##combined plot for per time point estimate and interval for replicated and original

#study 2

bind_s2 <-  bind_rows(s2_new,
                      s2_bind_rep_1 , .id = "id") %>% 
  mutate(id, fct_recode(id,"Original" = "1",
                   "Replicated" = "2")) %>% 
  select(-id) %>% 
  rename(Trend = `fct_recode(id, Original = "1", Replicated = "2")`,
         estimate = est,
         chi = upp,
         clow = low) %>% 
  mutate(Study = "Study 2") 

#study 4


bind_s4 <- bind_rows(s4_new_re,
                     s4_rep_round_summ_join_sel, .id = "id") %>% 
         mutate(id, fct_recode(id,"Original" = "1",
                               "Replicated" = "2")) %>% 
         select(-id) %>% 
         rename(Trend = `fct_recode(id, Original = "1", Replicated = "2")`) %>% 
         mutate(Study = "Study 4")
         

bind_s2 %>% 
  ggplot(aes(x = age, y = estimate, colour = Trend)) +
  geom_point(size = 4, shape = 19)+
  geom_line(linetype = 2)+
  geom_errorbar(aes(ymin = clow, ymax = chi), lineend = NULL, linewidth = 1,
                 alpha = 0.3) +
  labs(x = "cal yr BP", y  = "Estimate") +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
  scale_x_reverse()

#----------------------------------------------------------#
# 5. Save estimates with CI as csv files --------------------
#----------------------------------------------------------#

est_ci_all <- 
 list("s2_rep_round_summ_join", "s3_na_rep_summ_join",
       "s3_as_rep_round_summ_join", "s3_eu_rep_summ_join", "s4_rep_round_summ_join")

purrr::map(est_ci_all, readr::write_csv)

write_csv(s2_rep_round_summ_join, here("Data/Paper_1/data_supplementary/s2_est_ci.csv"))
write_csv(s3_na_rep_summ_join, here("Data/Paper_1/data_supplementary/s3_na_est_ci.csv"))
write_csv(s3_as_rep_round_summ_join, here("Data/Paper_1/data_supplementary/s3_as_est_ci.csv"))
write_csv(s3_eu_rep_summ_join, here("Data/Paper_1/data_supplementary/s3_eu_est_ci.csv"))
write_csv(s4_rep_round_summ_join, here("Data/Paper_1/data_supplementary/s4_est_ci.csv"))


