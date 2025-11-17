library(testthat)
library(tibble)
library(ggplot2)

test_that("bin_rarefy_estimate_richness_harmonized_data() validates input types", {
  expect_error(
    bin_rarefy_estimate_richness_harmonized_data(NULL),
    "data.frame"
  )
  
  expect_error(
    bin_rarefy_estimate_richness_harmonized_data(1),
    "data.frame"
  )
  
  expect_error(
    bin_rarefy_estimate_richness_harmonized_data(list(a = 1)),
    "data.frame"
  )
})

test_that("bin_rarefy_estimate_richness_harmonized_data() accepts valid data.frame", {
  df <- tibble(dataset_id = character(), age = numeric())
  expect_silent(bin_rarefy_estimate_richness_harmonized_data(df))
})

test_that("bin_rarefy_estimate_richness_harmonized_data() rejects missing required columns", {
  df1 <- tibble(dataset_id = "A")
  df2 <- tibble(age = 1000)
  df3 <- tibble(x = 1, y = 2)
  
  expect_error(
    bin_rarefy_estimate_richness_harmonized_data(df1)
  )
  expect_error(
    bin_rarefy_estimate_richness_harmonized_data(df2)
  )
  expect_error(
    bin_rarefy_estimate_richness_harmonized_data(df3)
  )
})

test_that("bin_rarefy_estimate_richness_harmonized_data() handles NA / edge cases in input", {
  df <- tibble(dataset_id = c("A", NA), age = c(1000, NA))
  expect_silent(bin_rarefy_estimate_richness_harmonized_data(df))
})

test_that("bin_rarefy_estimate_richness_harmonized_data() returns a ggplot object", {
  df <- tibble(dataset_id = character(), age = numeric())
  
  result <- bin_rarefy_estimate_richness_harmonized_data(df)
  
  expect_s3_class(result, "ggplot")
  expect_true("geom" %in% names(result$layers[[1]]))
})

test_that("bin_rarefy_estimate_richness_harmonized_data() produces plot with expected aesthetics", {
  df <- tibble(dataset_id = character(), age = numeric())
  
  plt <- bin_rarefy_estimate_richness_harmonized_data(df)
  
  expect_true("x" %in% names(plt$labels))
  expect_true("y" %in% names(plt$labels))
})
