library(testthat)
library(dplyr)
library(tidyr)

test_that("get_pollen_ages() validates input types", {
  expect_error(get_pollen_ages(NULL), "data_compilation")
  expect_error(get_pollen_ages(1), "data_compilation")
  expect_error(get_pollen_ages("a"), "data_compilation")
  expect_error(get_pollen_ages(list()), "data_compilation")
})

test_that("get_pollen_ages() validates required columns", {
  df_missing_levels <- data.frame(dataset_id = 1)
  expect_error(get_pollen_ages(df_missing_levels))
  
  df_missing_dataset_id <- data.frame(levels = I(list(data.frame(sample_id = 1, age = 10))))
  expect_error(get_pollen_ages(df_missing_dataset_id))
})

test_that("get_pollen_ages() validates levels structure", {
  df_bad_levels <- data.frame(dataset_id = 1, levels = "not_a_list")
  expect_error(get_pollen_ages(df_bad_levels))
  
  df_wrong_levels_df <- data.frame(
    dataset_id = 1,
    levels = I(list(data.frame(a = 1, b = 2)))
  )
  expect_error(get_pollen_ages(df_wrong_levels_df))
})

test_that("get_pollen_ages() returns a data.frame with expected columns", {
  df <- data.frame(
    dataset_id = c(1, 2),
    levels = I(list(
      data.frame(sample_id = c("a", "b"), age = c(10, 20)),
      data.frame(sample_id = "c", age = 30)
    ))
  )
  
  out <- get_pollen_ages(df)
  
  expect_s3_class(out, "data.frame")
  expect_named(out, c("dataset_id", "sample_id", "age"))
  expect_equal(nrow(out), 3)
})

test_that("get_pollen_ages() correctly unnests levels", {
  df <- data.frame(
    dataset_id = 1,
    levels = I(list(data.frame(sample_id = c("x", "y"), age = c(5, 15))))
  )
  
  out <- get_pollen_ages(df)
  
  expected <- data.frame(
    dataset_id = c(1, 1),
    sample_id = c("x", "y"),
    age = c(5, 15)
  )
  
  expect_equal(out, expected)
})

test_that("get_pollen_ages() handles zero-row levels", {
  df <- data.frame(
    dataset_id = 1,
    levels = I(list(data.frame(sample_id = character(), age = numeric())))
  )
  
  out <- get_pollen_ages(df)
  
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
  expect_named(out, c("dataset_id", "sample_id", "age"))
})
