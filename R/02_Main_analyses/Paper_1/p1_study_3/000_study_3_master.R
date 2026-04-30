library(tidyverse)
library(here)
library(assertthat)



source(
  here("R/02_Main_analyses/Paper_1/p1_study_3/00_subset_data.R")
)

source(
  here("R/02_Main_analyses/Paper_1/p1_study_3/01_harmonize_data.R")
)

source(
  here("R/02_Main_analyses/Paper_1/p1_study_3/02_rarefy_data.R")
)

source(
  here("R/02_Main_analyses/Paper_1/p1_study_3/03_add_random_selection_of_time.R")
  )

source(
  here("R/02_Main_analyses/Paper_1/p1_study_3/04_bin_data.R")
)

source(
  here("R/02_Main_analyses/Paper_1/p1_study_3/05_estimate_richness.R")
)

source(
  here("R/02_Main_analyses/Paper_1/p1_study_3/06_fit_model.R")
)

source(
  here("R/02_Main_analyses/Paper_1/p1_study_3/07_visualize_trend.R")
)

