#----------------------------------------------------------#
#               Holocene Diversity Project
#
#                       Novio & Mottl
#
#                          2026
#
# 
#
#      ----  Digitization of Plots (Study 1 - 4) ----
#----------------------------------------------------------#

## Load libraries

library(tidyverse)
library(here)
library(metaDigitise)

#----------------------------------------------------------#
# 1. Load figures -----------------------------------------
#----------------------------------------------------------# 

# 1.1. set directory

data_dig <-
  metaDigitise::metaDigitise(here("Data/Paper_1/data_digitise/re_dig/"), summary = FALSE)


#----------------------------------------------------------#
# 2. Load data from digitized figures --------------------
#----------------------------------------------------------# 

#Study 1
s1 <- data_dig$Fig_S1.png %>% 
  as_tibble()

s1

#Study 2

s2 <- data_dig$Fig_S2.png %>% 
  as_tibble()

s2

#Study 3

s3_eu <- data_dig$Fig_S3_EU.png %>% 
  as_tibble()

s3_eu

s3_na <- data_dig$Fig_S3_NA.png %>% 
  as_tibble()

s3_na


s3_as <- data_dig$Fig_S3_AS.png %>% 
  as_tibble()

s3_as

#Study 4

s4 <- data_dig$Fig_S4.png %>% 
  as_tibble()

s4

#----------------------------------------------------------#
# 3. Save data as csv files --------------------
#----------------------------------------------------------# 

write_csv(s1,here("Data/Paper_1/data_digitise/s1.csv"))
write_csv(s2,here("Data/Paper_1/data_digitise/s2.csv"))
write_csv(s3_eu,here("Data/Paper_1/data_digitise/s3_eu.csv"))
write_csv(s3_na,here("Data/Paper_1/data_digitise/s3_na.csv"))
write_csv(s3_as,here("Data/Paper_1/data_digitise/s3_as.csv"))
write_csv(s4,here("Data/Paper_1/data_digitise/s4.csv"))
