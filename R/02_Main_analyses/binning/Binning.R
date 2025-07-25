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


#----------------------------------------------------------#
# Binning sum pollen count per each taxa (across samples) within a specific time period-----
#----------------------------------------------------------#

#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#

library(tidyverse)
library(here)

data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

N_hemisphere <- data %>% 
  filter(region %in% c("North America", "Europe", "Asia")) %>% 
  relocate(region)


glimpse(N_hemisphere)

N_hemisphere_regions <- 
  N_hemisphere %>% 
  distinct(dataset_id, region)



#----------------------------------------------------------#
# 1. Obtain pollen counts and age -----
#----------------------------------------------------------#


pollen_counts <- N_hemisphere %>% 
  select(dataset_id, raw_counts) %>% 
  unnest(raw_counts) %>% 
  pivot_longer(
    cols = !c(dataset_id,sample_id),
    names_to = "taxa", values_to = "pollen_counts",
    values_drop_na = TRUE)

data_age <- N_hemisphere %>% 
  select(dataset_id, levels) %>% 
  unnest(levels) %>% 
  select(dataset_id,sample_id, age)

#--------------------------------------------------#
# 2. Binning -----
#--------------------------------------------------#


data_bin <- inner_join(pollen_counts, data_age,
                     by = c("dataset_id", 'sample_id')) %>% 
  mutate(BIN = cut(age, seq(min(age), 
                            max(age) + 500, 500), right = FALSE))%>%            # create BINS
  arrange(age)  # min age is -75, max age is 19992 (min-max = 20,067/500 = 40.134)


data_bin


data_bin_region <- inner_join(data_bin, N_hemisphere_regions, by = "dataset_id") # join binned data with regions

#--------------------------------------------------#
## 2.1. Check bins -----
#--------------------------------------------------#

data_bin_region%>% 
  count(BIN) %>% 
  print(n = 41)  

levels(data_bin_region$BIN)
#--------------------------------------------------#
## 2.2. Recode bins -----
#--------------------------------------------------#



data_bin_rec <- data_bin_region %>% 
  mutate(BIN_chr = as.character(BIN)) %>% 
  mutate(BIN_fct = as.factor(BIN_chr)) %>% 
  mutate(BIN_int = as.numeric(BIN_fct) %>% 
           as.factor()) 

levels(data_bin_rec$BIN_int)

data_bin_rec

#--------------------------------------------------#
## 2.3. Convert BIN from fct to dbl -----
#--------------------------------------------------#


data_bin_rec2 <- mutate_if(data_bin_rec, is.factor, ~ as.numeric(as.character(.x))) 

data_bin_rec2_clean <- data_bin_rec2 %>% select(!BIN)%>% select(!BIN_fct) %>% rename(BIN = BIN_int)


#--------------------------------------------------#
## 3. Build function -----
#--------------------------------------------------#

pollen_sum <- function(df, condition,var1,var2, var3){
  df %>% 
    filter({{condition}}) %>% 
    group_by({{var1}}, {{var2}}) %>% 
    summarise(summed_pollen_count = sum({{var3}})) 
}

data_bin_rec2_clean %>% pollen_sum(BIN ==2, taxa,region, pollen_counts)



#--------------------------------------------------#
## 3.1. Finalize function -----
#--------------------------------------------------#


sum_pollen_counts_by_bin_by_taxa_region <- function(df, bin) {
  pollen_sum(df, BIN == bin, taxa, region, pollen_counts)
}

sum_pollen_counts_by_bin_by_taxa_region(data_bin_rec2_clean, 2)


#--------------------------------------------------#
## 3.2.1 Create loop -----
#--------------------------------------------------#


data_bin_vec <- 
  data_bin_rec2_clean %>% distinct(BIN) %>% 
  pull(BIN)


data_bin_rec3 <- tibble::tibble()

res <- tibble::tibble()

for (x in data_bin_vec) {
  res <-
    data_bin_rec2_clean %>% 
    pollen_sum(BIN == x, taxa,region, pollen_counts) %>% 
    mutate(BIN = x)
  
  data_bin_rec3 <- 
    bind_rows(data_bin_rec3, res)
}


res


write_rds(res, here("Data/Processed/res.rds"))
#--------------------------------------------------#
# 3.2.2. Map -----
#--------------------------------------------------#

data_bin_2 <-
  data_bin_vec %>% 
  purrr::set_names() %>% 
  purrr::map(
    .progress = TRUE,
    .x = .,
    .f = ~ sum_pollen_counts_by_bin_by_taxa_region(data_bin_rec2_clean, bin = .x)
  ) %>% 
  bind_rows(.id = "BIN")


data_bin_2


write_rds(data_bin_2,here("Data/Processed/data_bin_2.rds"))



