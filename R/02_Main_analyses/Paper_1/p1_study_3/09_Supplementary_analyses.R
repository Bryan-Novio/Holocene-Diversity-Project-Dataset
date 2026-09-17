#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 3: Gordon et al
#
#                       
#                          2024
#
# North America & Europe, site-based richness (dataset_id,age, 
# 500 bins - rarefy 300 
#
#
#               ---- SUBSETTING DATA  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <-
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))



#----------------------------------------------------------#
# 2. Load functions ---------------------------------------
#----------------------------------------------------------#

# Get a vector of general functions

fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "\\.R$",
    recursive = TRUE
  )

# Load the function into the global environment

source_files <- 
  sapply(
    paste0("R/Functions/", fun_list, sep = ""),
    source
  )



############ spatial distribution of site ids 

# sub-setting data to Europe/N.America/Asia

study3_data  <-
  data %>% 
  relocate(region) %>% 
  filter(region %in% c("North America", "Europe", "Asia")
  )


asia <- study3_data %>% 
  filter(region =="Asia")  %>% 
  select(dataset_id, long, lat)


eur <- study3_data %>% 
  filter(region =="Europe")  %>% 
  select(dataset_id, long, lat)



namerica <- study3_data %>% 
  filter(region =="North America")  %>% 
  select(dataset_id, long, lat)



eur %>% 
  ggplot(aes(x = long, y = lat)) + 
  borders(fill= "gray") +
  geom_point( colour = "blue", size = 2) +
  coord_quickmap(xlim = c(-11, 35), ylim = c(36, 70))+
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")


namerica %>% 
  ggplot(aes(x = long, y = lat)) + 
  borders(fill= "gray") +
  geom_point( colour = "blue", size = 2) +
  coord_quickmap(xlim = c(-172, -56), ylim = c(28,74))+
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y =  element_blank(),
        axis.ticks.y =  element_blank(),
        axis.ticks.x =  element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")
