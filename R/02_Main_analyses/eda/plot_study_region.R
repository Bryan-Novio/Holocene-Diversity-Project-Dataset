
library(tidyverse)
library(here)

data <- 
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

p <- data %>% 
  ggplot(aes(x = long, y = lat)) +
  borders(fill= "gray") +
  geom_point(color ="#2A707F" ) +
  coord_quickmap(ylim = c(26,80)) +
  theme_classic(ink = "#FFFFFF") 
  

s <- p +
  labs(dictionary = c(
    region = "Region",
    lat = "Latitude",
    long = "Longitude")
    )

install.packages("tiff")

library("ggsave")


tiff('s', units="in", width=5, height=6, res=1200, compression = 'lzw')
ggsave(s, here("Outputs/Paper_1/pollen_sites_map.svg"))

  