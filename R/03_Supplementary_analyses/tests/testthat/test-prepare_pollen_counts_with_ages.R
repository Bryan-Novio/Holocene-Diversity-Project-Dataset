library(testthat)
library(tidyverse)
library(assertthat)

test_that("get_pollen_counts() accepts valid input data frame", {
  raw <- tibble(
    sample_id = c("21011", "21012"),
    abies = c(10, 20),
    alnus = c(5, NA)
  )
  df <- tibble(
    dataset_id = 1,
    raw_counts = list(raw)
  )
  
  expect_silent(res <- get_pollen_counts(df))
  expect_s3_class(res, "tbl_df")
})

test_that("get_pollen_counts() returns correct structure and names", {
  raw <- tibble(
    sample_id = c("21011", "21012"),
    abies = c(1, 2),
    alnus = c(3, 4)
  )
  df <- tibble(
    dataset_id = 10,
    raw_counts = list(raw)
  )
  res <- get_pollen_counts(df)
  
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c("dataset_id", "sample_id", "taxa", "pollen_counts"))
  expect_equal(nrow(res), 4)
  expect_equal(ncol(res), 4)
})


test_that("get_pollen_counts() handles NA taxa values by dropping them", {
  raw <- tibble(
    sample_id = "21011",
    alnus = NA_integer_,
    abies = 5
  )
  df <- tibble(
    dataset_id = 1001,
    raw_counts = list(raw)
  )
  res <- get_pollen_counts(df)
  
  expect_equal(unique(res$taxa), "abies")
  expect_equal(res$pollen_counts, 5)
})

test_that("get_pollen_counts() validates presence of required columns", {
  df_missing_raw <- tibble(dataset_id = 1001)
  expect_error(get_pollen_counts(df_missing_raw), "raw_counts")
  
  df_missing_dataset <- tibble(raw_counts = list(tibble(sample_id = "21011", count = 1)))
  expect_error(get_pollen_counts(df_missing_dataset), "dataset_id")
})


test_that("get_pollen_counts() errors on invalid types for data_compilation", {
  expect_error(get_pollen_counts(NULL))
  expect_error(get_pollen_counts(123))
  expect_error(get_pollen_counts("not a df"))
  expect_error(get_pollen_counts(list(a = 1)), "data frame or tibble") 
})

test_that("get_pollen_counts() errors when raw_counts is not a list of data frames", {
  data_1  <- tibble(
    dataset_id = 1001,
    raw_counts = 5
  )
  expect_error(get_pollen_counts(data_1), "Column `sample_id` doesn't exist") 
  
  data_2 <- tibble(
    dataset_id = 1001,
    raw_counts = list(5)
  )
  expect_error(get_pollen_counts(data_2), "Column `sample_id` doesn't exist") 
})

test_that("get_pollen_counts() handles empty raw_counts data frames", {
  df <- tibble(
    dataset_id = 1001,
    raw_counts = list(tibble(sample_id = character(), X = integer()))
  )
  
  res <- get_pollen_counts(df)
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 0)
  expect_equal(colnames(res), c("dataset_id", "sample_id", "taxa", "pollen_counts"))
})