#----------------------------------------------------------#
#               Holocene Diversity Project
#
#            Paper01| Method 1: Giesecke et al
#
#
# Europe, site-based richness (dataset_id,age), 1000 bins - 
# rarefy 500 
#                          2019
#
# 
#               ----SUPPLEMENTARY ANALYSES  ----
#
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

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

source_files <- sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)

#----------------------------------------------------------#
# 3. Subset data for Paper 1, Study 1
#----------------------------------------------------------# 

# subset pollen data to 25°W and 35°E longitude and north of 35°N latitude

data_p1_s1 <- data %>% filter(long >= -25 & long <= 35,lat >= 35) 


min(data_p1_s1$lat) #min lat
max(data_p1_s1$lat) # max lat

min(data_p1_s1$long) #min long
max(data_p1_s1$long) # max long

#----------------------------------------------------------#
# 3. Divide subset data into sub-regions
#----------------------------------------------------------# 

# assign data points to sub-regions

data_p1_s1_sub_region <- data_p1_s1  %>%
  mutate(subregion = case_when(
    lat > 57 ~ "Boreal", 
    lat >= 45 & lat <= 47 & long <= 15 & long >= 5 ~ "Alps", 
    lat < 45 ~ "Meridional/Submeridional", 
    long < 11 ~ "Temperate Oceanic", 
    long >= 11 ~ "Temperate Continental"
  )) %>%
  relocate(subregion, .after = region) 

# visualize in map

data_p1_s1_sub_region %>% 
ggplot(aes(x=long, y= lat, color = subregion)) +
  borders(fill= "gray", colour = "black") +
  geom_point() +
  scale_color_manual(values = c(
    "Alps" = "black",
    "Boreal" = "darkgreen",
    "Meridional/Submeridional" = "red",
    "Temperate Continental" = "orange",
    "Temperate Oceanic" = "blue"
  )) +
  coord_quickmap(xlim = c(-25,35), ylim = c(35,75))
  

data_p1_s1_sub_region %>% count(subregion)# check subregions


data_p1_s1_sub_region %>% filter(subregion == "Alps") %>% unnest(levels) %>% relocate(age,country) %>% distinct(country)
data_p1_s1_sub_region %>% filter(subregion == "Boreal") %>% unnest(levels) %>% relocate(age,country) %>% distinct(country)
data_p1_s1_sub_region %>% filter(subregion == "Meridional/Submeridional") %>% unnest(levels) %>% relocate(age,sample_id) %>% distinct(country)
data_p1_s1_sub_region %>% filter(subregion == "Temperate Continental") %>% unnest(levels) %>% relocate(age,country) %>% distinct(country)
data_p1_s1_sub_region %>% filter(subregion == "Temperate Oceanic") %>% unnest(levels) %>% relocate(age,country) %>% distinct(country)

#----------------------------------------------------------#
# 4. Get pollen counts with ages
#----------------------------------------------------------# 

data_p1_s1_subregion_counts_ages <- data_p1_s1_sub_region %>% 
  get_pollen_counts_with_ages() 

data_p1_s1_subregion_counts_ages %>% arrange(desc(age)) %>% head(10) # max. age


#                   ----HARMONIZATION ----
#----------------------------------------------------------#


#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

harmonization_table  <- read_csv(here("Data/harmonization_table_rev.csv"), show_col_types = FALSE)
neotoma_taxa <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)

#----------------------------------------------------------#
# 2. Test {harmonize_taxa} at different taxo rank --
#----------------------------------------------------------# 

taxa_level <- c("level_5", "level_6", "level_7") 
taxa_name <- c("family", "genus", "species")

# Harmonize taxa at different taxonomic levels

harmonized_data_study <- purrr::map(taxa_level, ~ harmonize_taxa(data_p1_s1_subregion_counts_ages, data_ancillary, .x)) %>%
  set_names(taxa_name)

harmonized_data_study$genus

#               ----  BINNING  ----
#----------------------------------------------------------#

harmonized_data_study$genus %>% bin_data(1000)

max(harmonized_data_study$genus$age)

#----------------------------------------------------------#
# 2. Bin data  at different taxo rank --
#----------------------------------------------------------# 

# Bin data


data_binned <-  harmonized_data_study$genus %>% 
  mutate(
    BIN = cut(age, seq(min(age), 
                      20000, 1000), right = FALSE),
    BIN_chr = as.character(BIN),
    BIN_fct = as.factor(BIN_chr),
    BIN_int = as.factor(as.numeric(BIN_fct)), # recode BINS to integer, then factor) 
    BIN = BIN_int) %>% 
  group_by(dataset_id ,sample_id, taxa, BIN, BIN_chr) %>% 
  summarise(summed_pollen_count = sum(pollen_counts), .groups = "drop")



# Prepare data for richness estimation

prepared_data_for_richness_estimation <- data_binned %>% 
  rename(
    age = BIN,
    pollen_grains = summed_pollen_count
  ) %>% 
  select(dataset_id, sample_id, age, taxa, pollen_grains) %>% 
  filter(pollen_grains > 0) %>% 
  mutate(age = as.numeric(age)  * 1000) 

max(prepared_data_for_richness_estimation$age)

#                    ----RAREFACTION  ----
#----------------------------------------------------------#
  
#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

prepared_data_for_richness_estimation %>% arrange(desc(age)) %>% head(10) # max. age

#----------------------------------------------------------#
# 2. Rarefy data  at different taxo rank --
#----------------------------------------------------------# 

all_samples <- unique(prepared_data_for_richness_estimation$sample_id)

# Apply rarefaction to all samples

results_list <- map(
  .x = all_samples,
  .f = ~ rarefy_pollen_grains_samp_iter(
    data_source = prepared_data_for_richness_estimation ,
    sample_id = .x,
    n_grains = 500,
    n_iter = 1),
  .progress = "TRUE")

results <- dplyr::bind_rows(results_list) %>%  # rarefied richness
  as_tibble()

age <- prepared_data_for_richness_estimation %>% select(age, sample_id) # get age

results_age_rarefied <- left_join(results, age, by ="sample_id", multiple = "any") # add age col to rarefied richness


#                 ---- RICHNESS ESTIMATION ----            #
#----------------------------------------------------------#

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

rarefied_data <- read_rds(here("Outputs/Data/paper_1_study_1/rarefied_data_study_1.rds"))

#----------------------------------------------------------#
# 2. Estimate richness  at different taxo rank --
#----------------------------------------------------------# 

richness <- results_age_rarefied %>%    # subregion richness
  mutate(present = ifelse(avg_n_pollen_grains >= 1, 1, 0)) %>% 
  group_by(dataset_id,age, sample_id) %>% 
  summarize(richness = sum(present, na.rm = TRUE, .groups = NULL))


data_subregion <- data_p1_s1_sub_region  %>% unnest(levels) %>%  select(subregion, sample_id)
  
richness_subregion <- left_join(richness, data_subregion, by ="sample_id") 

min(richness_subregion$age)
max(richness_subregion$age)

# compute median richness

median_richness_data <- richness_subregion  %>%
  group_by(age, subregion) %>%
  summarise(
    median_richness = median(richness, na.rm = TRUE),
    .groups = "drop"
  )

median_richness_15k <- median_richness_data %>% filter(age <= 15000)


## same

ggplot(median_richness_15k, aes(x = age, y = median_richness, color = subregion)) +
  geom_line(linewidth = 1) +
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(median_richness_15k, aes(x = age, y = median_richness, color = subregion)) +
  annotate("rect", xmin = Inf, xmax = 11500, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.2) +
  annotate("rect", xmin = 11500, xmax = 8500, ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.2) +
  annotate("rect", xmin = 8500, xmax = 4500, ymin = -Inf, ymax = Inf, fill = "lightyellow", alpha = 0.2) +
  annotate("rect", xmin = 4500, xmax = 0, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.2) +
  geom_line(size = 1) +
  scale_x_reverse(
    breaks = seq(0, 15000, by = 1000),
    labels = function(x) {
      if_else(x %in% c(0, 3000, 6000, 9000, 12000, 15000), as.character(x), "")
    }
  ) +
  scale_y_continuous(breaks = c(0, 20, 25, 30)) +
  scale_color_manual(values = c(
    "Alps" = "black",
    "Boreal" = "darkgreen",
    "Meridional/Submeridional" = "red",
    "Temperate Continental" = "orange",
    "Temperate Oceanic" = "blue"
  )) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Median site richness")


#========================================================#

#richness by time against altitude

#past 10K cal bp

richness_subregion_lat <- left_join(richness, subregion, by ="sample_id", multiple = "any") %>% 
  select(dataset_id.x,age.x,sample_id,richness, subregion, lat) %>% 
  rename(dataset_id = dataset_id.x, age = age.x)

ten <- richness_subregion_lat %>% 
  group_by(dataset_id) %>% 
  filter(age == 10000) %>% 
    ggplot(aes(x = lat, y = richness)) +
    geom_point(color = "blue")+
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    scale_x_continuous(position = "top") +
    geom_text(
    x = Inf,
    y = Inf,
    label = "10 ka",
    hjust = 1.1,
    vjust = 1.1,
    size = 4)+
    theme_classic()

#past 7K cal bp

sev  <- richness_subregion_lat %>% 
  group_by(dataset_id) %>% 
  filter(age == 7000) %>% 
  ggplot(aes(x = lat, y = richness)) +
  geom_point(color = "blue")+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_continuous(position = "top") +
  geom_text(
    x = Inf,
    y = Inf,
    label = "7 ka",
    hjust = 1.1,
    vjust = 1.1,
    size = 4)+
  theme_classic()

#past 4K cal bp

four <- richness_subregion_lat %>% 
  group_by(dataset_id) %>% 
  filter(age == 4000) %>% 
  ggplot(aes(x = lat, y = richness)) +
  geom_point(color = "blue")+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_continuous(position = "top") +
  geom_text(
    x = Inf,
    y = Inf,
    label = "4 ka",
    hjust = 1.1,
    vjust = 1.1,
    size = 4) +
    theme_classic()

#past 3K cal bp

three <- richness_subregion_lat %>% 
  group_by(dataset_id) %>% 
  filter(age == 3000) %>% 
  ggplot(aes(x = lat, y = richness)) +
  geom_point(color = "blue")+
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_continuous(position = "top") +
  geom_text(
    x = Inf,
    y = Inf,
    label = "3 ka",
    hjust = 1.1,
    vjust = 1.1,
    size = 4)+
  theme_classic()

#past 1K cal bp

one <- richness_subregion_lat %>% 
  group_by(dataset_id) %>% 
  filter(age == 1000) %>% 
  ggplot(aes(x = lat, y = richness)) +
  geom_point(color = "blue")+
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  scale_x_continuous(position = "top") +
  geom_text(
    x = Inf,
    y = Inf,
    label = "1 ka",
    hjust = 1.1,
    vjust = 1.1,
    size = 4
  ) +
  theme_classic() 

lat_trend <- ggarrange(ten,sev,four, three, one, common.legend = TRUE, nrow = 1, label.y = 1)

#===========================================================================================#

#Customize harmonization table for Study 1 by adding missing taxa

study1_hlist_raw <- 
  read_csv(here("C:/Users/ADMIN/Downloads/EMPD_Pollen/study1_hlist_raw.csv")) 

study1_hlist_raw %>% 
  distinct(taxon_name)

harmonisation_table <- 
  readr::read_csv(  
    here::here("Data/Paper_1/data_harmonize/harmonization_table_all_studies.csv")
  ) %>% 
  rename(taxon_name = neotoma_names) %>% 
  select(taxon_name, level_6) %>% 
  rename(Pollen_type = level_6)

harmonisation_table %>% distinct(taxon_name)

harmonisation_table_missing <- 
  study1_hlist_raw %>% 
  anti_join(harmonisation_table, by = "taxon_name")


study1_hlist_updated <- 
  bind_rows(harmonisation_table,harmonisation_table_missing)

study1_hlist_updated %>% 
  distinct(taxon_name)

View(study1_hlist_updated)

write_csv(study1_hlist_updated, here("Data/Paper_1/data_supplementary/study1_hlist_updated.csv"))


#===========================================================================================#


# View p1_study1 datasets (12/15/2025)

pollen_data_s1_renamed %>% 
  filter(pollen_counts > 0) %>% 
  group_by(dataset_id, age) %>% 
  summarise(n_taxa = n_distinct(taxon_name)) %>% 
  summary()


data_study1_harmonised %>% 
  filter(pollen_counts > 0) %>% 
  group_by(dataset_id, age) %>% 
  summarise(n_taxa = n_distinct(taxon_name)) %>% 
  summary()


pollen_data_s1_renamed$pollen_counts %>% 
  sum()


data_study1_harmonised$pollen_counts %>% # 30873834
  sum()


data_binned %>% 
  filter(summed_pollen_count > 0) %>% 
  group_by(dataset_id, BIN) %>% 
  summarise(n_taxa = n_distinct(taxa)) %>% 
  summary()

data_binned$summed_pollen_count %>%  # 30873834
  sum()

data_binned_samples_500$summed_pollen_count %>%  # 23968770
  sum()

data_binned_samples_500 %>% 
  filter(summed_pollen_count > 0) %>% 
  group_by(dataset_id, BIN) %>% 
  summarise(n_taxa = n_distinct(taxa)) %>% 
  summary()

data_binned_samples_500 %>% 
  summarise(n_taxa = n_distinct(taxa)) %>% 
  summary()

data_tesst_1 <- data_study1_harmonised %>% 
  group_by(dataset_id) %>% 
  summarise(new_pollen_sum = sum(pollen_counts))

data_tesst_2 <- data_binned %>% 
  group_by(dataset_id) %>% 
  summarise(new_pollen_sum = sum(summed_pollen_count))

full_join(data_tesst_1,data_tesst_2 , by ="dataset_id", suffix = c("harm", "binned")) %>% 
  mutate(pollen_difference = new_pollen_sumharm - new_pollen_sumbinned) %>% 
  filter(pollen_difference != 0)


rarefied_data1 %>% 
  filter(summed_pollen_count > 0) %>% 
  group_by(dataset_id, BIN) %>% 
  summarise(n_taxa = n_distinct(taxa)) %>% 
  summary()

#Study report ===========================================================================================#

#Number of dataset ids per time

n_id_time <- 
  richness %>%
  select(dataset_id,age) %>% 
  group_by(age) %>% 
  summarise(n_id_time = n())

n_id_time %>% 
  ggplot(aes(x = age, y = n_id_time))  +
  geom_segment(yend = min(n_id_time) , color = "blue",size = 0.5) +
  geom_point(color= "red",size = 3) +
  geom_area(fill = "lightyellow", alpha = 0.5)+
  theme_classic()
  
# Standard deviation(sd)

richness_stats <- 
  richness %>% 
  group_by(age) %>% 
  summarise(n_id_time = n(),
            mean_richness = mean(richness),
            stddev = sd(richness),
            mean_sd_l = mean_richness - stddev,
            mean_sd_u = mean_richness + stddev) 

# Mean richness per time

plot_mean <-
  richness_stats %>% 
  ggplot(aes(x = age, y = mean_richness)) +
  geom_point() +
  theme_classic()

# Add sd as error bars

with_error_bar <- 
  plot_mean + geom_errorbar(aes(ymin = mean_sd_l , ymax = mean_sd_u ), width=0.2)


with_error_bar

# Add jitters to the individual data points

add_each_data_point <-
  b + geom_point(data=richness, aes(x=age, y= richness), position = position_jitter(), color="skyblue",  alpha = 0.1) +
  theme_classic()

add_each_data_point




############ spatial distribution of site ids 


# RaW (bigger circ)


raw <- data %>% 
  filter(region == "Europe") %>% 
  select(dataset_id, long, lat)


# Analyzed (smaller circ)

richness <-
  read_csv(here("Data/Paper_1/data_estimate_richness/study1_richness.csv"))


an <- richness %>% 
  distinct(dataset_id) %>% 
  mutate(dataset_id = as.character(dataset_id))

an_coord <- 
  left_join(an, raw, by = "dataset_id")

raw_plus_an <- bind_rows(raw,an_coord,.id = "data")

raw_plus_an <- raw_plus_an %>% 
  mutate(data = fct_recode(data ,  Raw = "1", Analyzed = "2"))


# Plot #raw 472- analyzed 451


raw_plus_an %>% 
  ggplot(aes(x = long, y = lat)) + 
  borders(fill= "white") +
  geom_point(aes(colour = data, alpha = 0.1, size = factor(data,levels = c("Analyzed","Raw")))) +
  coord_quickmap(xlim = c(-11, 35), ylim = c(36, 70))+
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "gray"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")





