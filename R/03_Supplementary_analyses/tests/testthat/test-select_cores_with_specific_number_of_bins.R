library(testthat)
library(tidyverse)

test_that("select_cores_with_specific_number_of_bins() validates input types", {
  df <- data.frame(dataset_id = c("a","a","b"), BIN = c(1,2,1))
  
  expect_no_error(select_cores_with_specific_number_of_bins(df, 1))
  
  expect_error(select_cores_with_specific_number_of_bins("not_df", 1),
               "data_input has to be data.frame")
  
  expect_error(select_cores_with_specific_number_of_bins(list(a=1), 1))
  
  expect_error(select_cores_with_specific_number_of_bins(df, "x"))
  expect_error(select_cores_with_specific_number_of_bins(df, c(1,2)))
  expect_error(select_cores_with_specific_number_of_bins(df, NA))
})

test_that("select_cores_with_specific_number_of_bins() checks required columns", {
  df_missing_dataset <- data.frame(BIN = 1:3)
  df_missing_bin <- data.frame(dataset_id = c("a","b","c"))
  
  expect_error(select_cores_with_specific_number_of_bins(df_missing_dataset, 1))
  expect_error(select_cores_with_specific_number_of_bins(df_missing_bin, 1))
})

test_that("select_cores_with_specific_number_of_bins() returns data.frame with expected structure", {
  df <- data.frame(
    dataset_id = c("a","a","b","b","b"),
    BIN = c(1,2,1,2,3),
    value = 1:5
  )
  
  out <- select_cores_with_specific_number_of_bins(df, 2)
  
  expect_s3_class(out, "data.frame")
  expect_true("dataset_id" %in% names(out))
  expect_true("BIN" %in% names(out))
  expect_true("value" %in% names(out))
})

test_that("select_cores_with_specific_number_of_bins() selects datasets with at least n_bins distinct BIN values", {
  df <- data.frame(
    dataset_id = c("a","a","a","b","b","c"),
    BIN = c(1,1,2,1,1,3)
  )
  
  expected <- df[df$dataset_id %in% c("a"), ]
  
  out <- select_cores_with_specific_number_of_bins(df, 2)
  
  expect_equal(out[order(out$dataset_id, out$BIN), ],
               expected[order(expected$dataset_id, expected$BIN), ])
})

test_that("select_cores_with_specific_number_of_bins() handles edge cases", {
  df_single <- data.frame(dataset_id = "a", BIN = 1)
  
  out1 <- select_cores_with_specific_number_of_bins(df_single, 1)
  expect_equal(out1, df_single)
  
  out2 <- select_cores_with_specific_number_of_bins(df_single, 2)
  expect_equal(nrow(out2), 0)
  
  df_dup <- data.frame(dataset_id = c("a","a","a"), BIN = c(1,1,1))
  out_dup <- select_cores_with_specific_number_of_bins(df_dup, 1)
  expect_equal(out_dup, df_dup)
  
  out_dup2 <- select_cores_with_specific_number_of_bins(df_dup, 2)
  expect_equal(nrow(out_dup2), 0)
})

