#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                        Paper01
#
#
#
# North America & Europe, site-based richness (dataset_id,age, 
# 500 bins - rarefy 300 
#
#
#          ----  DATA OVERVIEW VISUALIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data overview subsets ---------------------------
#----------------------------------------------------------#

study1_data_overview <- 
  read_csv(here("Data/Paper_1/data_supplementary/data_overview/study1_data_overview.csv"))

study2_data_overview <- 
  read_csv(here("Data/Paper_1/data_supplementary/data_overview/study2_data_overview.csv"))

study3_data_overview <- 
  read_csv(here("Data/Paper_1/data_supplementary/data_overview/study3_data_overview.csv"))

study4_data_overview <- 
  read_csv(here("Data/Paper_1/data_supplementary/data_overview/study4_data_overview.csv"))

#----------------------------------------------------------#
# 2. Combine data overview subsets -----------------------
#----------------------------------------------------------#

data_overview_all_studies <- 
  bind_rows(study1_data_overview, study2_data_overview,
            study3_data_overview, study4_data_overview )

max(data_overview_all_studies$n_datasets)
min(data_overview_all_studies$n_datasets)

max(data_overview_all_studies$n_samples)
min(data_overview_all_studies$n_samples)

max(taxa$n_taxa)
min(taxa$n_taxa)

#----------------------------------------------------------#
# 3. Visualize  -----------------------
#----------------------------------------------------------#

# Plot all three


this_order <- c("raw", "select_woody_taxa", "harm", "rarefied", "rarefied_new_age", "binned", "richness")

data_overview_all_studies %>% 
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
  ggplot(aes(x = factor(step, levels = this_order), y = Count, colour = step)) +
  labs(x = "Step", color = "Step") +
  geom_point(size = 4) +
  theme_bw()+
  theme(axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1)) + 
  facet_grid(cols = vars(study_reg), rows = vars(metric), scales = "free", space = "free_x") 

