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


data_p1_s1 <- data %>% 
  filter(region =="Europe") %>%   # sub-setting data to Europe  - 25°W and 35°E long and north of 35°N latitude
  relocate(region)


data_p1_s1 %>% colnames() # check columns


min(data_p1_s1$lat) #min lat
max(data_p1_s1$lat) # max lat

min(data_p1_s1$long) #min long
max(data_p1_s1$long) # max long

########divide data into sub-regions

data_p1_s1_sub_region <- data_p1_s1 %>%
  mutate(subregion = case_when(
    lat > 57 ~ "Boreal",
    lat < 45 ~ "Meridional/Submeridional",
    lat == 40 & lat <= 60  ~ "Temperate Oceanic",
    lat == 40 & lat <= 60 & long >= -11 ~ "Temperate Continental",
    lat >= 45 & lat <= 47 & long >= -5 & long <= -15 ~ "Alps",
  )) %>% relocate(subregion, .after = region) %>% drop_na("subregion")

data_p1_s1_sub_region %>% count(subregion) # check subregions

#####3.1. get pollen counts with ages

data_p1_s1_counts_ages <- data_p1_s1 %>% get_pollen_counts_with_ages() 

data_p1_s1_counts_ages %>% arrange(desc(age)) %>% head(10) # max. age

                               




#               ----HARMONIZATION ----
#----------------------------------------------------------#

library(tidyverse)
library(here)


#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

pollen_data_s1 <-  read_rds(here("Outputs/Data/paper_1_study_1/datasub_p1_s1_counts_ages.rds"))
harmonization_table  <- read_csv(here("Data/harmonization_table_rev.csv"), show_col_types = FALSE)
neotoma_taxa <- readr::read_csv(here("Data/Input/Harmonisation_tables/taxa_reference_table_2025-01-24.csv"), show_col_types = FALSE)


pollen_data_s1 %>% arrange(desc(age)) %>% head(10) # max. age -> 19,810 cal BP
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
# 3. Test {harmonize_taxa} at different taxo rank --
#----------------------------------------------------------# 

taxa_level <- c("level_5", "level_6", "level_7") 
taxa_name <- c("family", "genus", "species")

# Harmonize taxa at different taxonomic levels

harmonized_data_study_1 <- purrr::map(taxa_level, ~ harmonize_taxa(pollen_data_s1, data_ancillary, .x)) %>%
  set_names(taxa_name)

#----------------------------------------------------------#
# Write the harmonized data to RDS files
write_rds(harmonized_data_study_1, here("Outputs/Data/paper_1_study_1/harmonized_data_study_1.rds"))
#----------------------------------------------------------#


#               ----  BINNING  ----
#----------------------------------------------------------#

library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

harmonized_data <- read_rds(here("Outputs/Data/paper_1_study_1/harmonized_data_study_1.rds"))

harmonized_data$genus %>% arrange(desc(age)) %>% head(10) # max. age
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
# 3. Bin data  at different taxo rank --
#----------------------------------------------------------# 


# Bin  data 

binned_data <- purrr::map(harmonized_data, ~ bin_data(.x, 1000)) 
# Prepare data for richness estimation

prepared_data_for_richness_estimation <- 
  purrr::map(binned_data, ~ prepare_data_for_richness_estimation(.x, "binned")) %>%
  purrr::map( ~ dplyr::mutate(.x, sample_id = paste0(dataset_id, "-", age)))

#----------------------------------------------------------#
# Write the binned and prepared_data to RDS files
write_rds(binned_data, here("Outputs/Data/paper_1_study_1/binned_data_study_1.rds"))
write_rds(prepared_data_for_richness_estimation, here("Outputs/Data/paper_1_study_1/prepared_data_for_richness_estimation_study_1.rds"))


#----RAREFACTION  ----
  #----------------------------------------------------------#
  
library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

prepared_data_for_richness_estimation <- read_rds(here("Outputs/Data/paper_1_study_1/prepared_data_for_richness_estimation_study_1.rds"))

prepared_data_for_richness_estimation$genus %>% arrange(desc(age)) %>% head(10) # max. age
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
# 3. Rarefy data  at different taxo rank --
#----------------------------------------------------------# 

rarefied_data <- purrr::map(prepared_data_for_richness_estimation, ~ rarefy_all_samples_iter(
  data_source =.,n_grains = 500, n_iter = 10)) %>% 
  purrr::map (~ separate_wider_delim(.x,sample_id, "-", names = c("sample_id","age")))


#----------------------------------------------------------#
# Write the rarefied data to an RDS file

write_rds(rarefied_data, here("Outputs/Data/paper_1_study_1/rarefied_data_study_1.rds"))
#----------------------------------------------------------#


#---- RICHNESS ESTIMATION ----
  #----------------------------------------------------------#
  
  library(tidyverse)
library(here)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

rarefied_data <- read_rds(here("Outputs/Data/paper_1_study_1/rarefied_data_study_1.rds"))

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
# 3. Estimate richness  at different taxo rank --
#----------------------------------------------------------# 

richness <- purrr::map(rarefied_data, ~ estimate_richness(.x)) %>% 
  purrr::map( ~ dplyr::mutate(.x,age = as.numeric(age)))


#----------------------------------------------------------#
# Write the richness data to an RDS file
write_rds(richness, here("Outputs/Data/paper_1_study_1/richness_data_study_1.rds"))
#----------------------------------------------------------#


#---- TREND VISUALIZATION ----
  #----------------------------------------------------------#
  
  library(tidyverse)
library(here)
library(ggplot2)
library(ggpubr)
library(mgcv)

#----------------------------------------------------------#
# 1. Load data set -----------------------------------------
#----------------------------------------------------------# 

richness <- read_rds(here("Outputs/Data/paper_1_study_1/richness_data_study_1.rds"))

#----------------------------------------------------------#
# 2. Visualize trends --
#----------------------------------------------------------# 

# 2.1. GLM model:

model_gen_glm <- glm(richness ~ age, data = richness$species)
model_gen_glm <- glm(richness ~ age, data = richness$genus)
model_gen_glm <- glm(richness ~ age, data = richness$species)

###### family


## 2.1.1. Data frame for predictions

new_data <- with(richness$family, data.frame(age = seq(min(age), max(age), length.out = 200)))

# 2.1.2. Predict the values and standard errors on the response scale

predictions <- predict(model_gen_glm, new_data, se.fit = TRUE, type = "response")

# 2.1.3. Add predictions and confidence intervals to the new data frame

new_data$fit <- predictions$fit
new_data$lwr <- predictions$fit - 1.96 * predictions$se.fit
new_data$upr <- predictions$fit + 1.96 * predictions$se.fit

# 2.1.4. Plot the results using ggplot2

ggplot(new_data, aes(x = age, y = fit)) +
  geom_point(data = richness$family, aes(y = richness), alpha = 0.5) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    x = "Age",
    y = "Predicted Richness"
  )


###### genus

## 2.1.1. Data frame for predictions

new_data <- with(richness$genus, data.frame(age = seq(min(age), max(age), length.out = 200)))

# 2.1.2. Predict the values and standard errors on the response scale

predictions <- predict(model_gen_glm, new_data, se.fit = TRUE, type = "response")

# 2.1.3. Add predictions and confidence intervals to the new data frame

new_data$fit <- predictions$fit
new_data$lwr <- predictions$fit - 1.96 * predictions$se.fit
new_data$upr <- predictions$fit + 1.96 * predictions$se.fit

# 2.1.4. Plot the results using ggplot2

ggplot(new_data, aes(x = age, y = fit)) +
  geom_point(data = richness$genus, aes(y = richness), alpha = 0.5) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    x = "Age",
    y = "Predicted Richness"
  )

###### species


## 2.1.1. Data frame for predictions

new_data <- with(richness$species, data.frame(age = seq(min(age), max(age), length.out = 200)))

# 2.1.2. Predict the values and standard errors on the response scale

predictions <- predict(model_gen_glm, new_data, se.fit = TRUE, type = "response")

# 2.1.3. Add predictions and confidence intervals to the new data frame

new_data$fit <- predictions$fit
new_data$lwr <- predictions$fit - 1.96 * predictions$se.fit
new_data$upr <- predictions$fit + 1.96 * predictions$se.fit

# 2.1.4. Plot the results using ggplot2

ggplot(new_data, aes(x = age, y = fit)) +
  geom_point(data = richness$species, aes(y = richness), alpha = 0.5) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    x = "Age",
    y = "Predicted Richness"
  )


# 2.2. GAM model:


# Assuming your model is already run
model_gam_identity <- gam(richness ~ s(age), data = richness$genus, family = gaussian(link = "identity"))

# Create a data frame for predictions
new_data <- with(richness$genus, data.frame(age = seq(min(age), max(age), length.out = 200)))

# Predict the smooth term
# Use type = "lpmatrix" to get the design matrix for the smooth term
# This allows you to reconstruct the effect with standard errors
lp_matrix <- predict(model_gam_identity, new_data, type = "lpmatrix")
smooth_effect <- lp_matrix %*% coef(model_gam_identity)      
smooth_se <- sqrt(rowSums((lp_matrix %*% vcov(model_gam_identity)) * lp_matrix))

# Construct the data frame for plotting
plot_data <- data.frame(
  age = new_data$age,
  fit = smooth_effect + coef(model_gam_identity)["(Intercept)"],
  lwr = smooth_effect + coef(model_gam_identity)["(Intercept)"] - 1.96 * smooth_se,
  upr = smooth_effect + coef(model_gam_identity)["(Intercept)"] + 1.96 * smooth_se
)

# Plot the results using ggplot2
ggplot(plot_data, aes(x = age, y = fit)) +
  # Add confidence interval ribbon
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  # Add smooth line
  geom_line() +
  # Add raw data points
  geom_point(data = richness$genus, aes(x = age, y = richness)) +
  # Add labels and title
  labs(
    title = "GAM Partial Effects Plot for 'age'",
    y = "Predicted Richness",
    x = "Age"
  ) +
  # Apply a clean theme
  theme_minimal()


######################



harmonized_data$genus %>% arrange(desc(age)) %>% head(10) 


harmonized_data$genus %>%  bin_data( 1000) %>% 
  prepare_data_for_richness_estimation("binned") %>%
   mutate( sample_id = paste0(dataset_id, "-", age)) %>% arrange(desc(age)) %>% head(10) 



harmonized_data$genus %>% filter(age <= 15000) %>%
  arrange(desc(age)) %>% 
  bin_data(1000) %>%
  prepare_data_for_richness_estimation("binned") %>%arrange(desc(age))




harmonized_data$genus %>% 
  mutate(
    BIN = cut(age, seq(min(age), 
                       max(age) + 1000, 1000), right = FALSE),
    BIN_chr = as.character(BIN),
    BIN_fct = as.factor(BIN_chr),
    BIN_int = as.factor(as.numeric(BIN_fct)), # recode BINS to integer, then factor) 
    BIN = BIN_int) %>% arrange(desc(age)) %>% head(10) %>% 
    filter




  
  
  group_by(dataset_id , taxa, BIN) %>% 
  summarise(summed_pollen_count = sum(pollen_counts), .groups = "drop") %>% 
  prepare_data_for_richness_estimation("binned") %>% arrange(desc(age)) %>% head(10) 
  
  
  purrr::map( ~ dplyr::mutate(.x, sample_id = paste0(dataset_id, "-", age)))
