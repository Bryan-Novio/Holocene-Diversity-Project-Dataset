##Test all R functions at once

setwd("C:/Users/ADMIN/Documents/Holocene-Diversity-Project-Dataset/R/03_Supplementary_analyses")

library(testthat)

testthat::test_dir(path="tests/testthat")
