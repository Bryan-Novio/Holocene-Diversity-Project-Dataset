library(tidyverse)
library(here)
library(testthat)
library(gratia)
library(marginaleffects)

fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "\\.R$",
    recursive = TRUE
  )

# Load the function into the global environment
source_files <-
  sapply(
    paste0("R/Functions/", fun_list, sep = ""),
    source
  )

# run all tests
testthat::test_dir(
  here("R/03_Supplementary_analyses/tests/testthat/")
)
