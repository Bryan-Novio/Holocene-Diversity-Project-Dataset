#----------------------------------------------------------#
#
#
#             Holocene Diversity Project Dataset
#
#                   Run all unit tests
#
#
#                   B. Novio, O. Mottl
#                         2026
#
#----------------------------------------------------------#

# Run from the project root:
#   Rscript R/03_Supplementary_analyses/tests/testthat.R
# Only the packages attached by the project config ({tidyverse}, {here})
#   are attached here, so the tests also catch functions that rely on
#   packages the project never attaches.


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(tidyverse)
library(here)


#----------------------------------------------------------#
# 1. Load functions -----
#----------------------------------------------------------#

vec_function_files <-
  list.files(
    path = here::here("R/Functions"),
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  )

purrr::walk(
  .x = vec_function_files,
  .f = ~ source(.x)
)


#----------------------------------------------------------#
# 2. Run tests -----
#----------------------------------------------------------#

testthat::test_dir(
  path = here::here("R/03_Supplementary_analyses/tests/testthat"),
  stop_on_failure = TRUE
)
