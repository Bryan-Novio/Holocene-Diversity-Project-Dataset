#----------------------------------------------------------#

library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <-
  read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

#----------------------------------------------------------#
# 2. Subset data per region---------------------------------
#----------------------------------------------------------# 

## 2.1. Subsetting 

raw_data_points_AS <- data %>% 
  select(dataset_id, long, lat, region) %>% 
  filter(region == "Asia")

raw_data_points_EU <- data %>% 
  select(dataset_id, long, lat, region) %>% 
  filter(region == "Europe") 

raw_data_points_NA <- data %>% 
  select(dataset_id, long, lat, region) %>% 
  filter(region == "North America")


##2.2. Convert subset to sf objects


raw_data_points_AS_sf <- raw_data_points_AS %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

raw_data_points_NA_sf <- raw_data_points_NA %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)


raw_data_points_EU_sf <- raw_data_points_EU %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326)


#----------------------------------------------------------#
# 3. Map pollen data on North Hemisphere--------------------
#----------------------------------------------------------# 

##3.1. Load world map

world <- ne_countries(scale = "medium", returnclass = "sf")

##3.2. Set focal point

sel <-  "Greenland"
country <-  world[world$name == sel, ]
ct <-  st_centroid(st_geometry(country)) #Center the view on focal point
coords <- st_coordinates(ct)
lon_0 <-  coords[[1, "X"]]

##3.2. Set CRS

ortho_crs  <-
  paste0("+proj=ortho +lat_0=90 +lon_0=20 +datum=WGS84 +units=m +no_defs"
, lon_0)


## 3.3.Visible hemisphere

horizon <-  st_buffer(ct, dist = 9900000)
world <-  st_intersection(world, horizon)
world_ortho <- st_transform(world, crs = ortho_crs)
world_ortho <-  st_make_valid(world_ortho)

# 3.4. Background ocean circle

world_coast = st_union(world_ortho)
ocean <- st_sfc(st_point(c(0, 0)), crs = ortho_crs)
ocean <- st_buffer(ocean, dist = 6371000)

g <- st_graticule(lat = seq(-90, 90, 20), lon = seq(-180, 180, 30), margin = 1e-5)# map graticule grid lines
g <- st_transform(g, crs = ortho_crs)


lon_ticks <- seq(-180, 150, 30)
lon_labels_df <- data.frame(lon = lon_ticks, lat = 0, label = paste0(lon_ticks, "°")) ## long. labels
lon_labels_sf <- st_as_sf(lon_labels_df, coords = c("lon", "lat"), crs = 4326)
lon_labels_sf <- st_transform(lon_labels_sf, crs = ortho_crs)


lat_ticks <- seq(20, 80, 20)
lat_labels_df <- data.frame(lon = -30, lat = lat_ticks, label = paste0(lat_ticks, "°N"))## lat.  labels
lat_labels_sf <- st_as_sf(lat_labels_df, coords = c("lon", "lat"), crs = 4326)
lat_labels_sf <- st_transform(lat_labels_sf, crs = ortho_crs)

## 3.5. Plot Northern hemisphere with data points(lat&long)

ggplot() +
  geom_sf(data = ocean, fill = "aliceblue", color = NA) +
  geom_sf(data = world_ortho, fill = "grey", color = "white") +
  geom_sf(data = g, color = alpha("grey", 0.3)) +
  # Scatter data points
  geom_sf(data = raw_data_points_AS_sf, aes(color = "Asia"), size = 2.5, alpha = 0.5) +
  geom_sf(data = raw_data_points_EU_sf, aes(color = "Europe"), size = 2.5,alpha = 0.5) +
  geom_sf(data = raw_data_points_NA_sf,aes(color = "North America"),size = 2.5,alpha = 0.5) +
  scale_color_manual(
    values = c(
      "Asia" = "red",
      "Europe" = "darkviolet",
      "North America" ="orange"
    )
  ) +
  labs (color = "Continent:") +
  theme(panel.background = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.title.y.left = element_blank(),
        legend.title = element_text(size = 18),
        axis.text.x = element_text(vjust = -0.9),
        axis.text.y = element_text(vjust = -0.5),
        legend.text = element_text(size = 15),
        legend.key.size = unit(1.2, "cm"),
        legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(size = 7)))



##END of SCRIPT


fig <-   tibble(continent = c("Asia","Europe","North America"),
         N_sites = c(55,472,474),
         N_samps = c(2821,45099, 27162)
         
         )

cols <-    c("Asia" = "red","Europe" = "purple","North America" = "orange")


fig2 <- fig %>% mutate(continent = as.factor(continent))


fig2 %>% 
  ggplot(aes(x= continent,y =  N_samps, fill = continent)) +
  geom_col() +
  theme_classic() +
  theme(axis.title.y = element_text(size = 30, margin = margin(r = 15)),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 25),
        axis.text.x = element_blank(),
        axis.ticks.x.bottom = element_blank(),
        legend.position = "none") +
  labs(y = "No. of Samples", x = "Continent") +
  scale_fill_manual(
    values = cols)

f+ scale_color_manual(
    values = c("Asia" = "red","Europe" = "purple","North America" = "orange")
  )





