#----------------------------------------------------------#
#
#
#               Holocene Diversity Project
#
#                       Binning
#
#               B.V. Novio & O. Mottl
#                        2025
#
#----------------------------------------------------------#
# Estimate richness per each 500  year bin within each record (core/dataset) within Northern hemisphere and plot it temporal trends per continent (region)
#----------------------------------------------------------#

#----------------------------------------------------------#
# 1. Richness by bin -----
#----------------------------------------------------------#

library(tidyverse)
library(here)

res <- read_rds(here("Data/Processed/res.rds"))
data_bin_2 <- read_rds(here("Data/Processed/data_bin_2.rds"))


View(data_bin_2 %>% filter(BIN == 23))

data_bin_2_richness <- data_bin_2 %>% 
  mutate(
    present = ifelse(summed_pollen_count >= 1, 1, 0)
  ) %>% 
  group_by(BIN, region) %>% 
  summarize(richness = sum(present, na.rm = TRUE, .groups = NULL))

#----------------------------------------------------------#
# 2. Plot temporal trends by point: BIN 1 -> -75 yrs, BIN 41 -> >20K BP -----
#----------------------------------------------------------#
#

data_bin_2_richness   %>% 
  ggplot(aes(y = richness, x = BIN, color = region)) + 
  geom_point() +
  scale_x_discrete(labels = c(1:41)) +
  theme_classic()

#----------------------------------------------------------#
# 3. Plot temporal trends by line and reordered bins 
#----------------------------------------------------------#


data_bin_2_richness  %>% 
  ggplot(aes(y = richness, x = as.factor(BIN),color = region, group = region)) + 
  geom_line(aes(color = region, fct_rev(BIN)))+
  scale_x_discrete(labels = c(41:1)) +
  xlab("Time Bins") +
  ylab("Richness") + 
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()


data_bin_2_richness %>% 
  select()
  mutate(avg = mean(richness))