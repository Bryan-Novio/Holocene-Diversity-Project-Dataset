library(testthat)
library(tidyverse)
library(assertthat)

test_that("prepare_data_for_richness_estimation() validates input types", {
  expect_error(prepare_data_for_richness_estimation(1, "binned"), "data_source")
  expect_error(prepare_data_for_richness_estimation("a", "binned"), "data_source")
  expect_error(prepare_data_for_richness_estimation(list(), "binned"), "data_source")
})

test_that("prepare_data_for_richness_estimation() validates required columns for type = 'binned'", {
  df <- data.frame(a = 1, b = 2)
  expect_error(
    prepare_data_for_richness_estimation(df, "binned"),
    "BIN"
  )
})

test_that("prepare_data_for_richness_estimation() validates required columns for other types", {
  df <- data.frame(a = 1, b = 2)
  expect_error(
    prepare_data_for_richness_estimation(df, "other"),
    "age"
  )
})

test_that("prepare_data_for_richness_estimation() works for type = 'binned' with valid input", {
  df <- data.frame(
    BIN = c(1, 2, 3),
    summed_pollen_count = c(10, 0, 5),
    dataset_id = c("1001", "1002", "15081"),
    taxa = c("abies", "alnus", "acer")
  )
  
  res <- prepare_data_for_richness_estimation(df, "binned")
  
  expect_s3_class(res, "data.frame")
  expect_named(res, c("dataset_id", "age", "taxa", "pollen_grains"))
  expect_equal(nrow(res), 2)
  expect_equal(res$age, c(1, 3) * 1000)
  expect_equal(res$pollen_grains, c(10, 5))
})

test_that("prepare_data_for_richness_estimation() works for non-binned type with valid input", {
  df <- data.frame(
    age = c(100, 200, 300),
    pollen_counts = c(10, 0, 5),
    dataset_id = c("1001", "1002", "15081"),
    taxa = c("abies", "alnus", "acer")
  )
  
  res <- prepare_data_for_richness_estimation(df, "raw")
  
  expect_s3_class(res, "data.frame")
  expect_named(res, c("dataset_id", "age", "taxa", "pollen_grains"))
  expect_equal(nrow(res), 2)
  expect_equal(res$age, c(100, 300))
  expect_equal(res$pollen_grains, c(10, 5))
})

test_that("prepare_data_for_richness_estimation() drops non-positive pollen counts", {
  df1 <- data.frame(
    BIN = 1:3,
    summed_pollen_count = c(3, 0, 2),
    dataset_id = "1001",
    taxa = c("abies", "alnus", "acer")
  )
  
  df2 <- data.frame(
    age = 1:3,
    pollen_counts = c(3, 0, 2),
    dataset_id = "1001",
    taxa = c("abies", "alnus", "acer")
  )
  
  res1 <- prepare_data_for_richness_estimation(df1, "binned")
  res2 <- prepare_data_for_richness_estimation(df2, "raw")
  
  expect_equal(nrow(res1), 2) 
  expect_equal(nrow(res2), 2)  
})


test_that("prepare_data_for_richness_estimation() handles zero-row data.frames", {
  df_binned <- data.frame(
    BIN = numeric(),
    summed_pollen_count = numeric(),
    dataset_id = character(),
    taxa = character()
  )
  
  expect_s3_class(
    prepare_data_for_richness_estimation(df_binned, "binned"),
    "data.frame"
  )
  
  df_raw <- data.frame(
    age = numeric(),
    pollen_counts = numeric(),
    dataset_id = character(),
    taxa = character()
  )
  
  expect_s3_class(
    prepare_data_for_richness_estimation(df_raw, "raw"),
    "data.frame"
  )
})

test_that("prepare_data_for_richness_estimation() rejects missing type values", {
  df <- data.frame(
    BIN = 1,
    summed_pollen_count = 143,
    dataset_id = "1001",
    taxa = "abies"
  )
  expect_error(prepare_data_for_richness_estimation(df, NA_character_))
})

test_that("prepare_data_for_richness_estimation() rejects NULL as data_source", {
  expect_error(prepare_data_for_richness_estimation(NULL, "binned"), "data_source")
})
