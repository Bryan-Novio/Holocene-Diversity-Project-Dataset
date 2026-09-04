#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 4: Bhatta et al
#
#
#                       
#                          2023
# Asia, site-based richness (dataset_id,age)
# nonbinned  - rarefy 300 
#
#            ---- SUPPLEMENTARY ----
#----------------------------------------------------------#

library(tidyverse)
library(here)


#----------------------------------------------------------#
# 1. Other visualizations ---------------------------------
#----------------------------------------------------------#

data <- 
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))


############ spatial distribution of site ids 


# RaW (bigger circ)


raw <- data %>% 
  filter(region =="Asia")  %>% 
  select(dataset_id, long, lat)


# Analyzed (smaller circ)


richness_data4 <- 
  read_rds(here("Data/Paper_1/data_estimate_richness/study4_richness.csv"))


an <- richness_data4 %>% 
  distinct(dataset_id) %>% 
  mutate(dataset_id = as.character(dataset_id))

an_coord <- 
  left_join(an, raw, by = "dataset_id")

raw_plus_an <- bind_rows(raw,an_coord,.id = "data")

raw_plus_an %>% distinct(data)

raw_plus_an <- raw_plus_an %>% 
  mutate(data = fct_recode(data ,  Raw = "1", Analyzed = "2"))


# Plot #raw 472- analyzed 451


raw_plus_an %>% 
  ggplot(aes(x = long, y = lat)) + 
  borders(fill= "gray") +
  geom_point(aes(colour = data, alpha = 0.1, size = factor(data,levels = c("Analyzed","Raw")))) +
  coord_quickmap(xlim = c(28, 180), ylim = c(20,80))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "gray"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

