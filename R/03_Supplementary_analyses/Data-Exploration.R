data(dune, package="vegan")

dune

str(dune)

readRDS()

library(tidyverse)
library(here)

data <- read_rds(here("Outputs/Data/data_assembly_2025-03-14__796c6bc270edcf0a682242164dd28a39__.rds"))

data

str(data)

glimpse(data)

View(data)

dim(data)



table(data$region)

data$region %>% table()


data %>% count(region)

data %>% 
  group_by(region) %>% 
  summarise(
    n = n()
  )

ggplot(
  data=data, aes(x=long, y = lat), 
) +
  ggplot2::borders() +
  geom_point(
    aes(col = region)
  ) +
  ggplot2::coord_quickmap()


mean(data$n_sample_counts)

data %>% 
  group_by(region) %>% 
  summarise(
  avg_s_count = mean(n_sample_counts),
  std_count = sd(n_sample_counts)
  )


data %>% 
  ggplot(aes(x = region, y = n_sample_counts)) +
  geom_boxplot()

class(data$n_sample_counts)
data$n_sample_counts
data$n_sample_counts[1]



data$raw_counts
class(data$raw_counts)
length(data$raw_counts)

class(data$raw_counts[1])

data$raw_counts[[1]]

class(data$raw_counts[[1]])

dim(data$raw_counts[[5]])

glimpse(data$raw_counts[[5]])

data$levels[[1]]

data %>% 
  select(dataset_id, region, levels) %>% 
  tidyr::unnest(levels)

data %>% 
  select(dataset_id, region, levels) %>% 
  tidyr::unnest(levels) %>% 
  group_by(region) %>% 
  summarise(
    min_age = min(age),
    max_age = max(age), 
    med_age = median(age),
    mean_age = mean(age)
  )

data %>% 
  select(dataset_id, region, levels) %>% 
  tidyr::unnest(levels) %>% 
  ggplot(aes(x = age, y = dataset_id)) +
  geom_point(size = 0.1)

data %>% 
  filter(region == "Europe") %>% 
  filter(altitude > 500)
