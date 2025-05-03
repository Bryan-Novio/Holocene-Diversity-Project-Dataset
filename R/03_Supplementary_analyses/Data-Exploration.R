
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

#EDA-Part I

glimpse(data)

# 1 How many unique datasets are available in total?
data %>% 
  distinct(dataset_id) %>% 
  summarize(n=n())

# 2 List all unique countries represented in the dataset.

data %>%
  distinct(country) %>% 
  count()
  

# 3 How many datasets are there per region?
data %>% 
  group_by(region) %>% 
  summarize(n=n())

# 4 How many datasets are there per country within each region?

x <- data %>% 
  count(region, country, sort = TRUE)

print(x, n = 82) # observed 3 NAs in country; 1 N.America (n=9), 1 Europe (n=2), 1 Africa (n=2); Indonesia in Oceania?

# 5 How many sites belong to each depositional environment?

data %>% 
  distinct(siteid, depositionalenvironment) %>% 
  group_by(depositionalenvironment) %>% 
  count()


# 6 Create a summary table of average altitude by region.

data %>% 
  group_by(region) %>% 
  summarize(mean_alt = mean(altitude, na.rm = TRUE)) %>% 
  arrange(desc(mean_alt)
  )


# 7 Plot a histogram of altitude values across all sites.

data$altitude

data %>% 
  ggplot(aes(x = altitude)) +
  geom_histogram()

data %>% 
  filter(altitude < 0)

data %>% 
  group_by(siteid) %>% 
  summarize(avg_altitude = mean(altitude)) %>% 
  ggplot(aes(x=avg_altitude)) +
  geom_histogram()

# 8 Which sites have the highest and lowest altitude values? highest -> at 4866, Monte Blanco (siteid 1717) lowest -> at  -218, 'Kinnerret'(site id 28120)

data %>% 
 group_by(siteid) %>%
  arrange(desc(altitude)
  )

tsiteid_test <- 
  data %>% 
    slice_max(altitude, n= 5) %>% 
    dplyr::pull(siteid)


data %>% 
  filter(altitude == max(altitude))

class(tsiteid_test)

tsiteid_test == "1717"

# 9 Plot site locations (long, lat) coloured by region.

data %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_point(
    aes(col = region)
  ) +
  ggplot2::borders() +
  ggplot2::coord_quickmap()
  


# 10 Which wwf_biome types are most and least represented? # temperate broadleaf and mixed forests

library(sf)
library(ggplot2)


biom <- read_sf("C:/Users/ADMIN/Documents/Holocene-Diversity-Project-Dataset/Data/Input/Spatial/Biomes_shapefile/WWF/wwf_terr_biomes.shp")
class(biom)
glimpse(biom)

names(data)

ggplot(data = biom) +
  geom_sf(aes(fill = BIOM_NAME)) +
  geom_point( data = data, aes(x = long, y = lat), color = "red")

#What is the earliest (min) and latest (max) age recorded in the entire dataset?

data %>%                        # latest  -75 BP
  select(dataset_id,age_min) %>% 
  arrange(age_min)

data %>% 
  filter(age_max == max(age_max)) %>% 
  dplyr::pull(siteid)

data %>% 
  filter(age_min == min(age_min)) %>% 
  dplyr::pull(siteid)
 
data %>%                        # earliest 19,9992 BP
  select(dataset_id,age_max) %>% 
  arrange(desc(age_max)
          )

#Calculate the maximum age_max for each region.

data %>% 
  group_by(region) %>% 
  summarize(max_age = max(age_max)
            ) %>% 
  arrange(desc(max_age)
          )

#Which sites span the longest chronological range (age_max - age_min)?

data %>% 
  group_by(region) %>% 
  mutate(lc_age = age_max - age_min) %>% 
  filter(lc_age == max(lc_age)) %>% 
  dplyr::select(lc_age)
  dplyr::pull(siteid) 
  
data %>% 
  ggplot(aes(y = dataset_id)) +
  geom_segment(
    aes(yend = dataset_id, xend = age_max, x = age_min)
  )
  
  
  

  

#Create a new column representing the temporal range and explore its distribution.

data %>% 
  mutate(lc_age = age_max - age_min, .before = 1) %>% 
  ggplot(aes( x = lc_age)) +
  geom_histogram (bins = 50) +
  theme_grey()

#What is the average and median number of samples (n_sample_counts) per dataset?

data %>% 
  group_by(region) %>% 
  summarise(mean_n_sample_counts = mean(n_sample_counts),
            median_n_sample_counts = median(n_sample_counts),
            total = sum(n_sample_counts)
            ) 

#Plot the distribution of n_sample_counts across all datasets.

data%>% 
  ggplot(aes(x = n_sample_counts)) +
  geom_histogram (bins = 50)+
  theme_grey()



#Identify the top 10 datasets with the highest number of samples.

data %>% 
  select(dataset_id, n_sample_counts) %>% 
  slice_max(n_sample_counts, n = 10)

#Unnest the levels column and count the total number of levels per region.

data %>% 
  unnest(levels) %>% 
  group_by(region) %>% 
  count()

#Calculate the mean number of levels per dataset, grouped by country.

data%>% 
  select(dataset_id, region, country, levels) %>% 
  unnest(levels) %>% 
  group_by(region, country) %>% 
  count(sort = TRUE) 


#Inspect the structure (column names, types) of one of the unnested levels tables. # 3chr # 17 dbl

data_3 <- 
  data %>% 
  select(dataset_id, country, levels) %>% 
  select(country) %>% 
  unnest(levels) 

data_3 <- 
  data %>% 
  select(dataset_id, country, levels) %>% 
  
  unnest(levels) 

 



glimpse(data_3)

#Which datasets have the most (max) and least (min) number of chronological control points (n_chron_control)?

   #max
data %>% 
  select(dataset_id, chron_control_format ) %>% 
  unnest(chron_control_format ) %>% 
  group_by(dataset_id) %>% 
  summarize(
    n_chron_control = n()) %>% 
 slice_max(n_chron_control)


   #min
data %>% 
  group_by(dataset_id) %>% 
  summarize(min_chron = min(n_chron_control)) %>% 
  arrange(min_chron)
  
 
#Summarise the average number of chronological control points per region.

data %>% 
  group_by(region) %>% 
  summarise(avg_chron = mean(n_chron_control)) %>% 
  arrange(desc(avg_chron))


data %>% 
  select(region, n_chron_control) %>% 
  group_by(region) %>% 
  summarize(avg = mean(n_chron_control))

#Unnest chron_control_format and explore the distribution of chronological methods used.


data %>% 
  unnest(chron_control_format) %>%
  ggplot(aes( y = fct_infreq(chroncontroltype))) +
  geom_bar()


data %>% 
  unnest(chron_control_format) %>%
  group_by(chroncontroltype) %>% 
  count() %>% 
  ggplot(aes( x = n, y = chroncontroltype)) +
  geom_bar(stat = "identity")

#Which curve_name values are used, and how frequently?

data %>% 
  unnest(chron_control_format) %>%
  ggplot(aes( y = fct_infreq(cal_curves))) +
  geom_bar()


#Filter datasets where age_min < 0 and explore which regions contain such records.


data %>% 
    filter(age_min < 0) %>% 
    relocate(age_min) %>% 
    group_by(region) %>% 
    summarize(n=n())


data %>% 
  filter(age_min < 0) %>% 
  relocate(age_min) %>% 
  group_by(region) %>% 
  summarize(n = n()
  )


data %>% 
  group_by(region) %>% 
  mutate(lc_age = age_max - age_min) %>% 
  filter(lc_age == max(lc_age)) %>% 
  dplyr::select(lc_age)
dplyr::pull(siteid) 


age_min_1 <- data %>% 
  group_by(dataset_id) %>% 
  unnest(age_min) %>% 
  unnest(age_min) %>% 
  filter(age_min < 0) %>% 
  relocate(age_min) %>% 
  group_by(region) 
   

ggplot(age_min_1, aes(x = age_min, y = fct_infreq(region))) +
  geom_col() +
  theme_classic()



#For datasets in the "Tundra" biome, how does n_sample_counts compare with other biomes?

library(sf)
library(ggplot2)

biom <- read_sf("C:/Users/ADMIN/Documents/Holocene-Diversity-Project-Dataset/Data/Input/Spatial/Biomes_shapefile/WWF/wwf_terr_biomes.shp")
class(biom)
glimpse(biom)

ggplot(data = biom) +
  geom_sf(aes(fill = BIOM_NAME)) +
  geom_point( data = data, aes(x = long, y = lat), color = "red") +
  geom_bar(data = data, )
                                                                                                                                                                

#(optional) Create a summary table with: dataset ID, site name, region, age range, and number of levels (from levels)(Hint: use mutate() with nrow() inside mutate() after unnest if needed)



data %>% 
  select(dataset_id, sitename, region, age_min, age_max, levels) %>% 
  mutate(age_range = age_max - age_min) %>% 
  relocate(age_range, .after = age_max) %>% 
  unnest(levels) 

   
  
data %>% 
  select(dataset_id, sitename, region, age_min, age_max, levels) %>%



