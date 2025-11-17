library(testthat)
library(dplyr)

test_that("estimate_richness() validates input types", {
  expect_error(estimate_richness(NULL), "data.frame")
  expect_error(estimate_richness(1), "data.frame")
  expect_error(estimate_richness("a"), "data.frame")
  expect_error(estimate_richness(list()), "data.frame")
})

test_that("estimate_richness() requires required columns", {
  df_missing_cols <- data.frame(a = 1, b = 2)
  expect_error(estimate_richness(df_missing_cols))
  
  df_missing_age <- data.frame(
    dataset_id = 1,
    avg_n_pollen_grains = 2
  )
  expect_error(estimate_richness(df_missing_age))
  
  df_missing_dataset <- data.frame(
    age = 1000,
    avg_n_pollen_grains = 5
  )
  expect_error(estimate_richness(df_missing_dataset))
  
  df_missing_pollen <- data.frame(
    dataset_id = 1,
    age = 1000
  )
  expect_error(estimate_richness(df_missing_pollen))
})

test_that("estimate_richness() works on valid input", {
  df <- data.frame(
    dataset_id = c(1, 1, 1, 2, 2),
    age = c(100, 100, 200, 100, 200),
    avg_n_pollen_grains = c(1, 0, 3, 2, 0)
  )
  
  expect_no_error(result <- estimate_richness(df))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("dataset_id", "age", "richness"))
})

test_that("estimate_richness() computes correct richness values", {
  df <- data.frame(
    dataset_id = c(1, 1, 1, 2, 2),
    age = c(100, 100, 200, 100, 200),
    avg_n_pollen_grains = c(1, 0, 3, 2, 0)
  )
  
  expected <- data.frame(
    dataset_id = c(1, 1, 2, 2),
    age = c(100, 200, 100, 200),
    richness = c(1, 1, 1, 0)
  )
  
  result <- estimate_richness(df)
  
  result <- result[order(result$dataset_id, result$age), ]
  expected <- expected[order(expected$dataset_id, expected$age), ]
  
  expect_equal(result, expected)
})

test_that("estimate_richness() handles NA values in avg_n_pollen_grains", {
  df <- data.frame(
    dataset_id = c(1, 1, 1),
    age = c(100, 100, 100),
    avg_n_pollen_grains = c(1, NA, 0)
  )
  
  expected <- data.frame(
    dataset_id = 1,
    age = 100,
    richness = 1
  )
  
  result <- estimate_richness(df)
  
  expect_equal(result, expected)
})

test_that("estimate_richness() handles zero-row data frame", {
  df <- data.frame(
    dataset_id = numeric(0),
    age = numeric(0),
    avg_n_pollen_grains = numeric(0)
  )
  
  result <- estimate_richness(df)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("dataset_id", "age", "richness"))
})

test_that("estimate_richness() handles negative or extreme values", {
  df <- data.frame(
    dataset_id = c(1, 1),
    age = c(100, 100),
    avg_n_pollen_grains = c(-5, 10)
  )
  
  expected <- data.frame(
    dataset_id = 1,
    age = 100,
    richness = 1
  )
  
  result <- estimate_richness(df)
  
  expect_equal(result, expected)
})
