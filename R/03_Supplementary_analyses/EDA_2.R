----------------------------------------------------------#
  #
  #
  #             Holocene Diversity Project
  #
  #              Data Exploration -2 Script
  #
  #               B.V. Novio &  O. Mottl
  #                        2025
  #
  #----------------------------------------------------------#
  

# Load up dataset & preliminary data processing

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

N_hemisphere %>% unnest(raw_counts)

# Estimate richness per each sample within each record (core/dataset) within Northern hemisphere and plot it temporal trends per continent (region).

data_richness <- N_hemisphere %>% 
  select(dataset_id, raw_counts) %>% 
  unnest(raw_counts) %>% 
  pivot_longer(
    cols = !c(dataset_id,sample_id),
    names_to = "taxa", values_to = "pollen_counts"
  ) %>% 
  mutate(
    present = ifelse(pollen_counts >= 1, 1, 0)
  ) %>% 
  group_by(dataset_id, sample_id) %>% 
  summarize(richness = sum(present, na.rm = TRUE))

data_richness

data_age <- N_hemisphere %>%                                 
  select(dataset_id, levels) %>% 
  unnest(levels) %>% 
  select(dataset_id,sample_id, age)

data_age

inner_join(data_richness, data_age, by = c("dataset_id", 'sample_id')) %>% 
  ggplot(aes(y = richness , x = age, group = dataset_id)) +
  geom_line() +
  theme_classic()

N_hemisphere$levels[[1]]


data_richnes_age <- inner_join(data_richness, data_age, by = c("dataset_id", 'sample_id'))
  
data_richness_age_region <- inner_join(data_richnes_age, N_hemisphere_regions, by = c("dataset_id"))

# temporal trends per region

data_richness_age_region %>% 
  ggplot(aes(y = richness, x = age, color = region)) + 
  geom_point() +
  geom_smooth(method = "gam", se = FALSE, size = 2) +
  theme_classic()

# Binning - sum pollen count per each taxa (across samples) within a specific time period. Make a function to do this

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


#Binning

binned <- inner_join(pollen_counts, data_age,
                     by = c("dataset_id", 'sample_id')) %>% 
          mutate(BIN = cut(age, seq(min(age), 
                    max(age) + 500, 500), right = FALSE))%>% 
          arrange(age)  # min age is -75, max age is 19992 (min-max = 20,067/500 = 40.134)
binned


binned_regions <- inner_join(binned, N_hemisphere_regions, by = "dataset_id")


#check bins created 
binned %>% 
  count(BIN) %>% 
  print(n = 41)  

#recode bins to bin nos.

bin_rec <- binned_regions %>% 
  mutate(
    BIN = fct_recode(BIN,
                     "1" = "[-75,425)",
                     "2" = "[425,925)",
                     "3" = "[925,1.42e+03)",
                     "4" = "[1.42e+03,1.92e+03)",
                     "5" = "[1.92e+03,2.42e+03)",
                     "6" = "[2.42e+03,2.92e+03)",
                     "7" = "[2.92e+03,3.42e+03)",
                     "8" = "[3.42e+03,3.92e+03)",
                     "9" = "[3.92e+03,4.42e+03)",
                     "10" = "[4.42e+03,4.92e+03)",
                     "11" = "[4.92e+03,5.42e+03)",
                     "12" = "[5.42e+03,5.92e+03)",
                     "13" = "[5.92e+03,6.42e+03)",
                     "14" = "[6.42e+03,6.92e+03)",
                     "15" = "[6.92e+03,7.42e+03)",
                     "16" = "[7.42e+03,7.92e+03)",
                     "17" = "[7.92e+03,8.42e+03)",
                     "18" = "[8.42e+03,8.92e+03)",
                     "19" = "[8.92e+03,9.42e+03)",
                     "20" = "[9.42e+03,9.92e+03)",
                     "21" = "[9.92e+03,1.04e+04)",
                     "22" = "[1.04e+04,1.09e+04)",
                     "23" = "[1.09e+04,1.14e+04)",
                     "24" = "[1.14e+04,1.19e+04)",
                     "25" = "[1.19e+04,1.24e+04)",
                     "26" = "[1.24e+04,1.29e+04)",
                     "27" = "[1.29e+04,1.34e+04)",
                     "28" = "[1.34e+04,1.39e+04)",
                     "29" = "[1.39e+04,1.44e+04)",
                     "30" = "[1.44e+04,1.49e+04)",
                     "31" = "[1.49e+04,1.54e+04)",
                     "32" = "[1.54e+04,1.59e+04)",
                     "33" = "[1.59e+04,1.64e+04)",
                     "34" = "[1.64e+04,1.69e+04)",
                     "35" = "[1.69e+04,1.74e+04)",
                     "36" = "[1.74e+04,1.79e+04)",
                     "37" = "[1.79e+04,1.84e+04)",
                     "38" = "[1.84e+04,1.89e+04)",
                     "39" = "[1.89e+04,1.94e+04)",
                     "40" = "[1.94e+04,1.99e+04)",
                     "41" = "[1.99e+04,2.04e+04)")
  )

bin_rec


#convert BIN from fct to dbl

bin_rec2 <- mutate_if(bin_rec, is.factor, ~ as.numeric(as.character(.x)))

bin_rec2


#basis of the function

taxa_sum <- bin_rec2 %>% 
  filter(BIN == 31) %>% 
  group_by(taxa) %>% 
  summarise(summed_pollen_count = sum(pollen_counts)) %>% 
  arrange(desc(summed_pollen_count))

taxa_sum

#build function

pollen_sum <- function(df, condition,var1,var2, var3){
  df %>% 
    filter({{condition}}) %>% 
    group_by({{var1}}, {{var2}}) %>% 
    summarise(summed_pollen_count = sum({{var3}})) 
}

bin_rec2 %>% pollen_sum(BIN ==2, taxa,region, pollen_counts)

#final function

sum_pollen_counts_by_bin_by_taxa_region <- function(df, bin) {
   pollen_sum(df, BIN == bin, taxa, region, pollen_counts)
}

sum_pollen_counts_by_bin_by_taxa_region(bin_rec2, 2)


# loop# 

vec_bins <- 
   bin_rec2 %>% distinct(BIN) %>% 
     pull(BIN)

bin_rec3 <- tibble::tibble()

for (x in vec_bins) {
  res <-
    bin_rec2 %>% 
    pollen_sum(BIN == x, taxa,region, pollen_counts) %>% 
    mutate(BIN = x)
  
  bin_rec3 <- 
    bind_rows(bin_rec3, res)
}

res

# map

data_binned <-
vec_bins %>% 
  purrr::set_names() %>% 
  purrr::map(
  .progress = TRUE,
  .x = .,
  .f = ~ sum_pollen_counts_by_bin_by_taxa_region(bin_rec2, bin = .x)
  ) %>% 
  bind_rows(.id = "BIN")

data_binned


# Estimate richness per each 500  year bin within each record (core/dataset) within Northern hemisphere and plot it temporal trends per continent (region). 


# richness by BIN

binned_data_richness <- data_binned  %>% 
  mutate(
    present = ifelse(summed_pollen_count >= 1, 1, 0)
  ) %>% 
  group_by(BIN, region) %>% 
  summarize(richness = sum(present, na.rm = TRUE, .groups = NULL))

#plot by point: BIN 1 -> -75 yrs, BIN 41 -> >20K BP
  
binned_data_richness  %>% 
  ggplot(aes(y = richness, x = BIN, color = region)) + 
  geom_point() +
  scale_x_discrete(labels = c(1:41)) +
  theme_classic()

#plot by line and reordered bins 
binned_data_richness %>% 
  ggplot(aes(y = richness, x = as.factor(BIN),color = region, group = region)) + 
  geom_line(aes(color = region, fct_rev(BIN)))+
  scale_x_discrete(labels = c(41:1)) +
  xlab("Time Bins") +
  ylab("Richness") + 
  geom_smooth(method = "gam", se = FALSE, size = 2) +
  theme_classic()




