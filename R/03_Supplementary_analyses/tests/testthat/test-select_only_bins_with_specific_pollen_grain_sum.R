library(testthat)
library(tidyverse)
library(assertthat)

test_that("select_only_bins_with_specific_pollen_grain_sum() validates input types", {
  df <- data.frame(BIN = c(1,1,2), summed_pollen_count = c(5,10,2))
  
  expect_no_error(select_only_bins_with_specific_pollen_grain_sum(df, 5))
  
  expect_error(select_only_bins_with_specific_pollen_grain_sum("x", 5),
               "data_input has to be a data.frame")
  
  expect_error(select_only_bins_with_specific_pollen_grain_sum(df, "x"))
  expect_error(select_only_bins_with_specific_pollen_grain_sum(df, c(1,2)))
  expect_error(select_only_bins_with_specific_pollen_grain_sum(df, NA))
})

test_that("select_only_bins_with_specific_pollen_grain_sum() checks required columns exist", {
  df_missing_bin <- data.frame(summed_pollen_count = 1:3)
  df_missing_count <- data.frame(BIN = c(1,1,2))
  
  expect_error(select_only_bins_with_specific_pollen_grain_sum(df_missing_bin, 5))
  expect_error(select_only_bins_with_specific_pollen_grain_sum(df_missing_count, 5))
})

test_that("select_only_bins_with_specific_pollen_grain_sum() output structure is correct", {
  df <- data.frame(
    BIN = c(1,1,2,3),
    summed_pollen_count = c(5,10,2,20),
    extra = letters[1:4]
  )
  
  out <- select_only_bins_with_specific_pollen_grain_sum(df, 10)
  
  expect_s3_class(out, "data.frame")
  expect_true(all(c("BIN", "summed_pollen_count", "extra") %in% names(out)))
})

test_that("select_only_bins_with_specific_pollen_grain_sum() filters bins by pollen sum", {
  df <- data.frame(
    BIN = c(1,1,2,2,3),
    summed_pollen_count = c(5,5,1,2,20)
  )
  
  expected_bins <- c(1,3)
  out <- select_only_bins_with_specific_pollen_grain_sum(df, 10)
  
  expect_equal(sort(unique(out$BIN)), expected_bins)
})

test_that("select_only_bins_with_specific_pollen_grain_sum() handles edge cases", {
  df_single <- data.frame(BIN = 1, summed_pollen_count = 5)
  out <- select_only_bins_with_specific_pollen_grain_sum(df_single, 5)
  expect_equal(out, df_single)
  
  out2 <- select_only_bins_with_specific_pollen_grain_sum(df_single, 10)
  expect_equal(nrow(out2), 0)
  
  df_zero <- data.frame(BIN = c(1,1,2), summed_pollen_count = c(0,0,0))
  out3 <- select_only_bins_with_specific_pollen_grain_sum(df_zero, 1)
  expect_equal(nrow(out3), 0)
  
  df_na <- data.frame(BIN = c(1,1,2), summed_pollen_count = c(5, NA, 10))
  out4 <- select_only_bins_with_specific_pollen_grain_sum(df_na, 10)
  expect_equal(sort(unique(out4$BIN)), 2)
})
