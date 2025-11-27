library(tidyverse)
library(purrr)
library(testthat)


test_that("bin_data() accepts valid inputs and returns expected structure", {
  
  df <- tibble(
    pollen_counts = c(0, 6, 1, 0),
    age = c(2000, 4000, 6000, 20000),
    taxa = c("abies", "acer", "alnus", "amaranthanceae"),
    dataset_id = c("15081", "15081", "15081", "15081")
  )
  
  res <- bin_data(df, binning_var = dataset_id, bin_size = 1000)
  
  expect_s3_class(res, "data.frame")
  
  expected_cols <- c("dataset_id", "taxa", "BIN", "BIN_chr", "summed_pollen_count")
  expect_true(all(expected_cols %in% colnames(res)))
  
  expect_true(all(res$summed_pollen_count >= 0))
})


test_that("bin_data() bins by the correct variable and aggregates pollen counts correctly", {
  
  df <- tibble(
    age = c(1050, 1203, 1284, 1317),
    pollen_counts = c(10, 30, 5, 15),
    taxa = c("abies", "acer", "alnus", "amaranthanceae"),
    dataset_id = c("15081", "1541", "16111", "17328")
  )
  
  res <- bin_data(df, binning_var = dataset_id, bin_size = 1000)
  
  breaks <- seq(min(df$age), max(df$age) + 1000, 1000)
  expected_bins <- cut(df$age, breaks, right = FALSE)
  
  expected_tbl <- df %>%
    mutate(
      BIN = expected_bins,
      BIN_chr = as.character(BIN),
      BIN_fct = factor(BIN_chr),
      BIN_int = factor(as.numeric(BIN_fct)),
      BIN = BIN_int
    ) %>%
    group_by(dataset_id, taxa, BIN, BIN_chr) %>%
    summarise(summed_pollen_count = sum(pollen_counts), .groups = "drop")
  
  expect_equal(
    arrange(res, BIN_chr)$summed_pollen_count,
    arrange(expected_tbl, BIN_chr)$summed_pollen_count
  )
})


 test_that("bin_data() rejects non-data.frame inputs for data_source", {
  expect_error(bin_data(5, binning_var = dataset_id, bin_size = 1000))
  expect_error(bin_data("text", binning_var = dataset_id, bin_size = 1000))
  expect_error(bin_data(NULL, binning_var = dataset_id, bin_size = 1000))
})


test_that("bin_data() rejects data_source missing required columns", {
  df_bad1 <- tibble(age = 1:3, taxa = c("abies", "acer", "alnus"))
  df_bad2 <- tibble(pollen_counts = 1:3, taxa = c ("abies", "acer", "alnus"))
  df_bad3 <- tibble(age = 1:3, pollen_counts = 1:3)
    
  expect_error(bin_data(df_bad1, binning_var = taxa, bin_size = 1000))
  expect_error(bin_data(df_bad2, binning_var = taxa, bin_size = 1000))
  expect_error(bin_data(df_bad3, binning_var = taxa, bin_size = 1000))
})


test_that("bin_data() rejects invalid bin_size values", {
  df <- tibble(
    age = 1:5,
    pollen_counts = 1:5,
    taxa = c("abies", "acer", "alnus", "amaranthanceae", "pinus"),
    dataset_id = "15081"
  )
  
  expect_error(bin_data(df, binning_var = site, bin_size = -1))
  expect_error(bin_data(df, binning_var = site, bin_size = 0))
  expect_error(bin_data(df, binning_var = site, bin_size = NA))
  expect_error(bin_data(df, binning_var = site, bin_size = "2"))
  expect_error(bin_data(df, binning_var = site, bin_size = c(1, 2)))
})


test_that("bin_data() handles single-row data.frame correctly", {
  df <- tibble(
    age = 5,
    pollen_counts = 10,
    taxa = "abies",
    dataset_id = "10581"
  )
  
  res <- bin_data(df, binning_var = dataset_id, bin_size = 1000)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$summed_pollen_count, 10)
})


test_that("bin_data() errors when binning_var column is missing", {
  df <- tibble(
    age = 1:3,
    pollen_counts = 1:3,
    taxa = c("abies", "acer", "alnus"),
    dataset_id = c("15081", "1541", "16111")
  )
  
  expect_error(bin_data(df, binning_var = site_id, bin_size = 1))
})


test_that("bin_data() works when binning_var has multiple groups", {
  
  df <- tibble(
    age = c(1, 2, 1, 2),
    pollen_counts = c(10, 20, 5, 5),
    taxa = c("abies", "acer", "alnus", "amaranthanceae"),
    dataset_id = c("15081", "1541","15081", "1541")
  )
  
  res <- bin_data(df, binning_var = dataset_id, bin_size = 1000)
  
  expect_equal(length(unique(res$dataset_id)), 2)
  expect_true(all(res$summed_pollen_count >= 0))
})


test_that("bin_data() handles NA values in age or pollen_counts appropriately", {
  df <- tibble(
    age = c(1, NA, 3),
    pollen_counts = c(10, 20, NA),
    taxa = c("abies", "acer", "alnus"),
    dataset_id = c("1541","1541","1541")
  )
  
  expect_error(bin_data(df, binning_var = dataset_id, bin_size = 1))
})

