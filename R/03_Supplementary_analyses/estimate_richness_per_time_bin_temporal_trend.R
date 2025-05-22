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

binned_data_richness <- data_binned  %>% 
  mutate(
    present = ifelse(summed_pollen_count >= 1, 1, 0)
  ) %>% 
  group_by(BIN, region) %>% 
  summarize(richness = sum(present, na.rm = TRUE, .groups = NULL))

#----------------------------------------------------------#
# 2. Plot temporal trends by point: BIN 1 -> -75 yrs, BIN 41 -> >20K BP -----
#----------------------------------------------------------#
#

binned_data_richness  %>% 
  ggplot(aes(y = richness, x = BIN, color = region)) + 
  geom_point() +
  scale_x_discrete(labels = c(1:41)) +
  theme_classic()

#----------------------------------------------------------#
# 3. Plot temporal trends by line and reordered bins 
#----------------------------------------------------------#


binned_data_richness %>% 
  ggplot(aes(y = richness, x = as.factor(BIN),color = region, group = region)) + 
  geom_line(aes(color = region, fct_rev(BIN)))+
  scale_x_discrete(labels = c(41:1)) +
  xlab("Time Bins") +
  ylab("Richness") + 
  geom_smooth(method = "gam", se = FALSE, size = 2) +
  theme_classic()
