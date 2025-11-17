library(testthat)
library(dplyr)

test_that("bin_data() accepts valid inputs and returns expected structure", {
  df <- tibble(
    age = c(1, 2, 5, 6),
    pollen_counts = c(10, 20, 5, 5),
    taxa = c("A", "A", "B", "B"),
    site = c("X", "X", "Y", "Y")
  )
  
  res <- bin_data(df, binning_var = site, bin_size = 2)
  
  expect_s3_class(res, "data.frame")
  expect_true(all(c("site", "taxa", "BIN", "BIN_chr", "summed_pollen_count") %in% colnames(res)))
  expect_true(all(res$summed_pollen_count >= 0))
})

test_that("bin_data() bins by the correct variable and aggregates pollen counts correctly", {
  df <- tibble(
    age = c(1, 1.5, 3, 4),
    pollen_counts = c(10, 30, 5, 15),
    taxa = c("A", "A", "A", "A"),
    region = c("R1", "R1", "R1", "R1")
  )
  
  res <- bin_data(df, binning_var = region, bin_size = 2)
  
  expected_bins <- cut(df$age, seq(min(df$age), max(df$age) + 2, 2), right = FALSE)
  expected_tbl <- tibble(
    region = "R1",
    taxa = "A",
    BIN_chr = as.character(expected_bins),
    BIN = as.factor(as.numeric(as.factor(BIN_chr))),
    summed_pollen_count = tapply(df$pollen_counts, expected_bins, sum, simplify = TRUE)
  ) %>%
    as_tibble() %>%
    filter(!is.na(BIN_chr))
  
  expect_equal(
    arrange(res, BIN_chr)$summed_pollen_count,
    arrange(expected_tbl, BIN_chr)$summed_pollen_count
  )
})

test_that("bin_data() rejects non-data.frame inputs for data_source", {
  expect_error(bin_data(5, binning_var = x, bin_size = 2))
  expect_error(bin_data("text", binning_var = x, bin_size = 2))
  expect_error(bin_data(NULL, binning_var = x, bin_size = 2))
})

test_that("bin_data() rejects data_source missing required columns", {
  df_bad1 <- tibble(age = 1:3, taxa = c("A", "A", "B"))
  df_bad2 <- tibble(pollen_counts = 1:3, taxa = c("A", "A", "B"))
  df_bad3 <- tibble(age = 1:3, pollen_counts = 1:3)
  
  expect_error(bin_data(df_bad1, binning_var = taxa, bin_size = 1))
  expect_error(bin_data(df_bad2, binning_var = taxa, bin_size = 1))
  expect_error(bin_data(df_bad3, binning_var = taxa, bin_size = 1))
})

test_that("bin_data() rejects invalid bin_size values", {
  df <- tibble(
    age = 1:5,
    pollen_counts = 1:5,
    taxa = c("A", "A", "A", "A", "A"),
    site = "X"
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
    taxa = "A",
    loc = "Z"
  )
  
  res <- bin_data(df, binning_var = loc, bin_size = 3)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$summed_pollen_count, 10)
})

test_that("bin_data() errors when binning_var column is missing", {
  df <- tibble(
    age = 1:3,
    pollen_counts = 1:3,
    taxa = c("A", "B", "C"),
    site = c("X", "X", "X")
  )
  
  expect_error(bin_data(df, binning_var = region, bin_size = 1))
})

test_that("bin_data() works when binning_var has multiple groups", {
  df <- tibble(
    age = c(1, 2, 1, 2),
    pollen_counts = c(10, 20, 5, 5),
    taxa = c("A", "A", "A", "A"),
    grp = c("G1", "G1", "G2", "G2")
  )
  
  res <- bin_data(df, binning_var = grp, bin_size = 2)
  
  expect_equal(length(unique(res$grp)), 2)
  expect_true(all(res$summed_pollen_count >= 0))
})

test_that("bin_data() handles NA values in age or pollen_counts appropriately", {
  df <- tibble(
    age = c(1, NA, 3),
    pollen_counts = c(10, 20, NA),
    taxa = c("A", "A", "A"),
    unit = c("U1", "U1", "U1")
  )
  
  expect_error(bin_data(df, binning_var = unit, bin_size = 1))
})
