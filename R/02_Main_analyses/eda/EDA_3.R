#----------------------------------------------------------#
#
#
#               Holocene Diversity Project
#
#       Binning, Richness Estimation & Rarefaction
#
#               B.V. Novio & O. Mottl
#                       
#                          2025
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# Functions for compilation and binned data                 
#----------------------------------------------------------#


library(tidyverse)
library(here)


data_1 <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))


#----------------------------------------------------------#
# 1. Load functions ---------------------------------------
#----------------------------------------------------------#

# Get a vector of general functions
fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "*.R",
    recursive = TRUE
  )

# Load the function into the global environment

sapply(
  paste0("R/Functions/", fun_list, sep = ""),
  source
)


#----------------------------------------------------------#
# 2. Binning  ---------------------------------------------
#----------------------------------------------------------#

data_binned_500 <- 
  get_pollen_counts_with_ages(data) %>% 
  bin_data(., 500)   # bin_size = 500          
                                          # binned data
data_binned_1000 <- 
  get_pollen_counts_with_ages(data) %>% 
  bin_data(., 1000)   # bin_size = 500  

data_not_binned <-
  get_pollen_counts_with_ages(data) # nonbinned data


#----------------------------------------------------------#
# 3. Prepare data for richness estimation -----------------
#----------------------------------------------------------#

data_for_richness_estimation_binned_500 <- prepare_data_for_richness_estimation(data_binned_500, "binned")
data_for_richness_estimation_binned_1000 <- prepare_data_for_richness_estimation(data_binned_1000, "binned")
data_for_richness_estimation_unbinned <- prepare_data_for_richness_estimation(data_not_binned, "nonbinned")

summary(data_for_richness_estimation_binned_500)
summary(data_for_richness_estimation_binned_1000)
summary(data_for_richness_estimation_unbinned)

#----------------------------------------------------------#
# 4. Estimate richness by  age(real or bin) ---------------
#----------------------------------------------------------#


richness_age_binned_500 <-  estimate_richness(data_for_richness_estimation_binned_500)
richness_age_binned_1000 <-  estimate_richness(data_for_richness_estimation_binned_1000)
richness_age_unbinned <-  estimate_richness(data_for_richness_estimation_unbinned)

summary(richness_age_binned_500)
summary(richness_age_binned_1000)
summary(richness_age_unbinned)



get_pollen_counts_with_ages(data) %>% 
  bin_data(bin_size = 500) %>% 
  prepare_data_for_richness_estimation(type = "binned") %>% 
  estimate_richness()

data %>% 
  filter(region == "Europe") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 1000) %>% 
  prepare_data_for_richness_estimation(type = "binned") %>% 
  estimate_richness()

get_pollen_counts_with_ages(data) %>% 
  prepare_data_for_richness_estimation(type = "nonbinned") %>% 
  estimate_richness()

#----------------------------------------------------------#
# 5. Rarefaction ------------------------------------------
#----------------------------------------------------------#

#----------------------------------------------------------#
# 5.1.1. For each individual sample 
#----------------------------------------------------------#

data_source <- data

# Filter the full dataset to keep only the specified sample
pollen_sample <-
  data %>% 
  get_pollen_counts_with_ages() %>%   
  filter(sample_id == 21042)

# Check pollen counts
pollen_sample %>% 
  group_by(taxa) %>% 
  summarise(p_count = sum(pollen_counts)) %>%   
  arrange(-p_count)

set.seed(1234)

rarefied_pollen_sample <- rarefy_pollen_grains_sample(data, 21042, 100) 

#----------------------------------------------------------#
# 5.1.2. All samples within a dataset 
#----------------------------------------------------------#

set.seed(1234)

rarefied_all_samples <-  data %>% 
  filter(region == "Europe") %>% 
   get_pollen_counts_with_ages() %>% 
    rarefy_all_samples(
     data_source =.,
     n_grains = 300)

#----------------------------------------------------------#
# 5.2.1. Within each sample - with  iteration
#----------------------------------------------------------#

 rarefy_pollen_grains_samp_iter(data,622157,300,100)
 
 #----------------------------------------------------------#
 # 5.2.2. All samples within each dataset  - with  iteration
 #----------------------------------------------------------#
 
 rarefy_pollen_generic(data, 1030, 300, 100)
 

 #---------------------------------------------------------------------------
 


# region, binned  - rarefy 300 - richness by dataset_id

set.seed(1234)


#asia
 
data %>% 
  filter(region == "Asia") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 500) %>% 
  prepare_data_for_richness_estimation(type = "nonbinned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 300,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age"))%>% 
   estimate_richness() %>% 
   ggplot(aes(y = richness, x =  age)) + 
   geom_point() +
   geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
   theme_classic()

# europe

data %>% 
  filter(region == "Europe") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 500) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate_wider_delim(sample_id, "-", names = c("sample_id","age")) %>% 
  estimate_richness() %>% 
  arrange(age) %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic() 

  
data %>% 
  filter(region == "Europe") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 500) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE) %>% 
  estimate_richness() %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()



#==============================================================
#w/o points
data %>% 
  filter(region == "Europe") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 1000) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE) %>% 
  estimate_richness() %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

#==============================================================

library(ggpubr)

# All studies in one plot

#Giesecke et al

s1 <- data %>% 
  filter(region == "Europe") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 1000) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE)%>% 
  estimate_richness() %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()

#Simova et al

s2 <- data %>% 
  filter(region == "North America") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 1000) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 300,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE) %>% 
  estimate_richness() %>% 
  ggplot2:::ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic() 


#Gordon et al

s3 <- data %>% 
  filter(region == "North America") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 500) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 300,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE)%>% 
  estimate_richness() %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()


#Bhatta et al

s4 <- data %>% 
  filter(region == "Asia") %>% 
  get_pollen_counts_with_ages() %>% 
  prepare_data_for_richness_estimation(type = "nonbinned" ) %>% 
  
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 300,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE) %>% 
  estimate_richness() %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()


# A- Europe, B- N.America, C- N. America, D - Asia

sc <- ggarrange(s1,s2,s3,s4,
                labels = c("A", "B", "C", "D"),
                ncol = 2, nrow = 2)

# A-D - N. America (visualize all methods in one continent)


s1_NA <- data %>% 
  filter(region == "North America") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 1000) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE)%>% 
  estimate_richness() %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()



s2_NA <- data %>% 
  filter(region == "North America") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 1000) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 300,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE) %>% 
  estimate_richness() %>% 
  ggplot2:::ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic() 


s3_NA <- data %>% 
  filter(region == "North America") %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 500) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 300,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE)%>% 
  estimate_richness() %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()


s4_NA <- data %>% 
  filter(region == "NOrth America") %>% 
  get_pollen_counts_with_ages() %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 300,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE) %>% 
  estimate_richness() %>% 
  ggplot(aes(y = richness, x =  age)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()


sc_NA <- ggarrange(s1_NA,s2_NA,s3_NA,
                labels = c("A", "B", "C"),
                ncol = 2, nrow = 2)
sc_NA

# regional richness--Giesecke et al


region <- data %>% distinct(dataset_id,region)

region
  
data %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 1000) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE)%>% 
  estimate_richness() %>% 
  inner_join(., region, by = "dataset_id") %>% 
  ggplot(aes(y = richness, x =  age, group = region, colour = region)) + 
  geom_point() +
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()


#without points

data %>% 
  get_pollen_counts_with_ages() %>% 
  bin_data(bin_size = 1000) %>% 
  prepare_data_for_richness_estimation(type = "binned" ) %>% 
  mutate(sample_id = paste0(dataset_id,"-",age)) %>% 
  rarefy_all_samples_iter(
    data_source =.,
    n_grains = 500,
    n_iter = 10) %>% 
  separate(sample_id, into = c("sample_id", "age"), sep = "-", convert = TRUE)%>% 
  estimate_richness() %>% 
  inner_join(., region, by = "dataset_id") %>% 
  ggplot(aes(y = richness, x =  age, group = region, colour = region)) + 
  geom_smooth(method = "gam", se = TRUE, linewidth = 0.5,) +
  theme_classic()
