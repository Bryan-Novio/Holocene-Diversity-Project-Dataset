

library(tidyverse)
library(testthat)

#----------------------------------------------------------#
# 1. Test 1: Input Type Validation -------------------------
#----------------------------------------------------------#

test_that("bin_data() accepts valid inputs and returns expected structure", {
  df <- tibble(
    age = c(1050, 1203, 1284, 1317),
    pollen_counts = c(0, 6, 1, 0),
    taxa = c("abies", "acer", "alnus", "amaranthanceae"),
    dataset_id = c("15081", "1541", "16111", "17328")
  )

  res <- bin_data(df, binning_var = dataset_id, bin_size = 1000)

  expect_s3_class(res, "data.frame")
  expect_true(all(c("dataset_id", "taxa", "BIN", "BIN_chr", "summed_pollen_count") %in% colnames(res)))
  expect_true(all(res$summed_pollen_count >= 0))
})

#----------------------------------------------------------#
# 2. Test 2: Binning by the correct variable and pollen count aggregation  --------
#----------------------------------------------------------#

test_that("bin_data() bins by the correct variable and aggregates pollen counts correctly", {
  df <- tibble(
    age = c(1050, 1203, 1284, 1317),
    pollen_counts = c(10, 30, 5, 15),
    taxa = c("abies", "acer", "alnus", "amaranthanceae"),
    dataset_id = c("15081", "1541", "16111", "17328")
  )
  
  res <- bin_data(df, binning_var = dataset_id, bin_size = 1000)

  expected_bins <- cut(df$age, seq(min(df$age), max(df$age) + 1000, 1000), right = FALSE)
  
  expected_tbl <- df %>%
    mutate(
      expected_bins = cut(
        age,
        seq(min(age), max(age) + 1000, 1000),
        right = FALSE
      ),
      BIN_chr = as.character(expected_bins),
      BIN_fct = as.factor(BIN_chr),
      BIN_int = as.factor(as.numeric(BIN_fct)),
      BIN = BIN_int
    ) %>%
    group_by(dataset_id, taxa, BIN, BIN_chr) %>%
    summarise(summed_pollen_count = sum(pollen_counts), .groups = "drop")
  
  expect_equal(
    arrange(res, BIN_chr)$summed_pollen_count,
    arrange(expected_tbl, BIN_chr)$summed_pollen_count
  )
})

#----------------------------------------------------------#
# 3. Test 3: Rejection of non-data.frame inputs ---------
#----------------------------------------------------------#

test_that("bin_data() rejects non-data.frame inputs for data_source", {
  expect_error(bin_data(5, binning_var = dataset_id, bin_size = 1000))
  expect_error(bin_data("text", binning_var = dataset_id, bin_size = 1000))
  expect_error(bin_data(NULL, binning_var = dataset_id, bin_size = 1000))
})

#----------------------------------------------------------#
# 4. Test 4: Check missing required cols  ---------
#----------------------------------------------------------#

test_that("bin_data() rejects data_source missing required columns", {
  df_bad1 <- tibble(age = 1:3, taxa = c("abies", "acer", "alnus"))
  df_bad2 <- tibble(pollen_counts = 1:3, taxa = c ("abies", "acer", "alnus"))
  df_bad3 <- tibble(age = 1:3, pollen_counts = 1:3)
    
  expect_error(bin_data(df_bad1, binning_var = taxa, bin_size = 1000))
  expect_error(bin_data(df_bad2, binning_var = taxa, bin_size = 1000))
  expect_error(bin_data(df_bad3, binning_var = taxa, bin_size = 1000))
})

#----------------------------------------------------------#
# 5. Test 5: Rejection if bin_size values are invalid ------
#----------------------------------------------------------#

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

#----------------------------------------------------------#
# 6. Test 6: handling of single-row data frame is correct  ------
#----------------------------------------------------------#

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


#----------------------------------------------------------#
# 7. Test 7: Binning_var column must be present  ------
#----------------------------------------------------------#
test_that("bin_data() errors when binning_var column is missing", {
  df <- tibble(
    age = 1:3,
    pollen_counts = 1:3,
    taxa = c("abies", "acer", "alnus"),
    dataset_id = c("15081", "1541", "16111")
  )
  
  expect_error(bin_data(df, binning_var = site_id, bin_size = 1))
})

#----------------------------------------------------------#
# 8. Test 8: Binning_var should work with multi-groups  ------
#----------------------------------------------------------#

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

#----------------------------------------------------------#
# 9. Test 9:  Handling NAs  ------
#----------------------------------------------------------#

test_that("bin_data() handles NA values in age or pollen_counts appropriately", {
  df <- tibble(
    age = c(1, NA, 3),
    pollen_counts = c(10, 20, NA),
    taxa = c("abies", "acer", "alnus"),
    dataset_id = c("1541","1541","1541")
  )
  
  expect_error(bin_data(df, binning_var = dataset_id, bin_size = 1))
})

