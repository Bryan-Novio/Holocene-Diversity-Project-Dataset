#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                        Paper01
#
#                   Study 1, 2, 3 & 4
#
#
#
#          ----  DATA OVERVIEW VISUALIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data overview subsets ---------------------------
#----------------------------------------------------------#

study_data_overview <- list.files(
  "Data/Paper_1/data_supplementary/data_overview",
  pattern = "[.]csv$",
  full.names = TRUE
)

#----------------------------------------------------------#
# 2. Combine data overview subsets -----------------------
#----------------------------------------------------------#

overview_all <- bind_rows(study_data_overview %>% 
            purrr::map(
              .f = ~ {
                overview <- read_csv(.x) 
              }
           )
          )

#----------------------------------------------------------#
# 3. Visualize data overview  -----------------------------
#----------------------------------------------------------#


order_vec <-
  c("raw", "select_woody_taxa", "harm", "rarefied",
    "rarefied_new_age", "binned", "richness")

overview_all %>% 
  select(study, step, n_datasets,  n_samples, n_taxa, region) %>% 
  tidyr::unite("study_reg", c(study, region), sep = "_", remove = TRUE) %>% 
  pivot_longer(
    cols = starts_with("n_"),
    names_to = "metric",
    names_prefix = "n_",
    values_to = "Count"
  ) %>% 
  mutate(study_reg = stringr::str_replace(study_reg,"_NA",""),
         study_reg = stringr::str_replace(study_reg,"_North America","_NA"),
         study_reg = stringr::str_replace(study_reg,"_Europe","_EU"),
         study_reg = stringr::str_replace(study_reg,"_Asia","_AS")) %>% 
  drop_na() %>%
  ggplot(aes(x = factor(step, levels = order_vec), y = Count, colour = step)) +
  geom_boxplot() +
  labs(x = "Step", color = "Step") +
  theme_bw()+
  theme(axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         legend.position = "bottom",
         legend.text = element_text(size = 6)) + 
  guides(colour = guide_legend(nrow = 1)) + 
  facet_grid(cols = vars(study_reg), rows = vars(metric), scales = "free", space = "free_x") 


study_data_overview[[1]] %>% 
  read_csv() %>% 
  pivot_longer(cols = starts_with("n"), 
               names_to = "metric", values_to = "count") %>% 
  ggplot(aes(x = step, y = count, colour = step)) +
  geom_boxplot() + 
  geom_point() +
  labs(x = "Step", color = "Step") +
  theme_bw()




study_data_overview[[2]] %>% 
  read_csv() %>% 
  pivot_longer(cols = starts_with("n"), 
               names_to = "metric", values_to = "count") %>% 
  ggplot(aes(x = step, y = count, colour = step)) +
  geom_boxplot() + 
  geom_point() +
  labs(x = "Step", color = "Step") +
  theme_bw()


study_data_overview[[3]] %>% 
  read_csv() %>% 
  pivot_longer(cols = starts_with("n"), 
               names_to = "metric", values_to = "count") %>% 
  ggplot(aes(x = step, y = count, colour = step)) +
  geom_boxplot() + 
  geom_point() +
  labs(x = "Step", color = "Step") +
  theme_bw()


# study 4

s4_order <-
  c("raw", "harm", "rarefied", "richness")

study_data_overview[[4]] %>% 
  read_csv() %>% 
  pivot_longer(cols = starts_with("n"), 
               names_to = "metric", values_to = "count") %>% 
  ggplot(aes(x = factor(step, levels = s4_order), y = count)) +
  geom_point(aes(colour = step), size = 4)+
  facet_wrap(~metric) +
  labs(x = "Step", color = "Step", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12))




# study 1

s1_order <-
  c("raw", "harm", "rarefied","binned", "richness")

study_data_overview[[1]] %>% 
  read_csv() %>% 
  pivot_longer(cols = starts_with("n"), 
               names_to = "metric", values_to = "count") %>% 
  ggplot(aes(x = factor(step, levels = s1_order), y = count)) +
  geom_point(aes(colour = step), size = 4)+
  facet_wrap(~metric) +
  labs(x = "Step", color = "Step", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12))



# study 2

s2_order <-
  c("raw", "binned","select_woody_taxa","harm", "rarefied", "richness")

study_data_overview[[2]] %>% 
  read_csv() %>% 
  pivot_longer(cols = starts_with("n"), 
               names_to = "metric", values_to = "count") %>% 
  ggplot(aes(x = factor(step, levels = s2_order), y = count)) +
  geom_point(aes(colour = step), size = 4)+
  facet_wrap(~metric) +
  labs(x = "Step", color = "Step", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 7)
        ) +
  guides(colour = guide_legend(nrow=1))

# Study 3


s3_order <-
  c("raw", "harm","rarefied","rarefied_new_age", "richness")

study_data_overview[[3]] %>% 
  read_csv() %>% 
  filter(region == "Europe") %>% 
  pivot_longer(cols = starts_with("n"), 
               names_to = "metric", values_to = "count") %>% 
  ggplot(aes(x = factor(step, levels = s3_order), y = count)) +
  geom_boxplot(aes(colour = step, height = 50))+
  facet_wrap(~metric) +
  labs(x = "Step", color = "Step", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  ) + 
  guides(colour = guide_legend(nrow=1))

study_data_overview[[3]] %>% 
  read_csv() %>% 
  filter(region == "Asia") %>% 
  ggplot(aes(x = step, y = n_samples)) +
  geom_boxplot()
