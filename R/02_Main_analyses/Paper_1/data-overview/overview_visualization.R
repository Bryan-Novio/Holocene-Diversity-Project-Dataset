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

