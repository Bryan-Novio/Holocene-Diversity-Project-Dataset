library(testthat)
library(dplyr)

test_that("get_pollen_counts_with_ages() validates input types", {
  expect_error(
    get_pollen_counts_with_ages(1),
    "data_compilation has to be data.frame"
  )
  expect_error(
    get_pollen_counts_with_ages(NULL),
    "data_compilation has to be data.frame"
  )
  expect_error(
    get_pollen_counts_with_ages(list(a = 1)),
    "data_compilation has to be data.frame"
  )
})

test_that("get_pollen_counts_with_ages() accepts valid data.frame", {
  df <- data.frame(x = 1)
  expect_error(get_pollen_counts_with_ages(df), NA)
})

test_that("get_pollen_counts_with_ages() returns correct structure", {
  df <- data.frame(a = 1)
  
  mocked_counts <- data.frame(
    dataset_id = c("d1", "d2"),
    sample_id = c("s1", "s2"),
    count = c(10, 20)
  )
  
  mocked_ages <- data.frame(
    dataset_id = c("d1", "d2"),
    sample_id = c("s1", "s2"),
    age = c(1000, 2000)
  )
  
  local_mocked_bindings(
    get_pollen_counts = function(x) mocked_counts,
    get_pollen_ages = function(x) mocked_ages
  )
  
  out <- get_pollen_counts_with_ages(df)
  
  expect_true(is.data.frame(out))
  expect_named(out, c("dataset_id", "sample_id", "count", "age"))
  expect_equal(nrow(out), 2)
})

test_that("get_pollen_counts_with_ages() performs correct inner join", {
  df <- data.frame(a = 1)
  
  mocked_counts <- data.frame(
    dataset_id = c("d1", "d2", "d3"),
    sample_id = c("s1", "s2", "s3"),
    count = c(10, 20, 30)
  )
  
  mocked_ages <- data.frame(
    dataset_id = c("d1", "d3"),
    sample_id = c("s1", "s3"),
    age = c(1000, 3000)
  )
  
  local_mocked_bindings(
    get_pollen_counts = function(x) mocked_counts,
    get_pollen_ages = function(x) mocked_ages
  )
  
  out <- get_pollen_counts_with_ages(df)
  
  expect_equal(nrow(out), 2)
  expect_equal(out$dataset_id, c("d1", "d3"))
  expect_equal(out$sample_id, c("s1", "s3"))
  expect_equal(out$count, c(10, 30))
  expect_equal(out$age, c(1000, 3000))
})

test_that("get_pollen_counts_with_ages() handles empty joins", {
  df <- data.frame(a = 1)
  
  mocked_counts <- data.frame(
    dataset_id = c("d1"),
    sample_id = c("s1"),
    count = 10
  )
  
  mocked_ages <- data.frame(
    dataset_id = c("d2"),
    sample_id = c("s2"),
    age = 2000
  )
  
  local_mocked_bindings(
    get_pollen_counts = function(x) mocked_counts,
    get_pollen_ages = function(x) mocked_ages
  )
  
  out <- get_pollen_counts_with_ages(df)
  
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 0)
  expect_named(out, c("dataset_id", "sample_id", "count", "age"))
})

