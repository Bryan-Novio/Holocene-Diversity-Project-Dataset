library(testthat)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(vegan)

test_that("rarefy_all_samples() validates input types", {
  expect_error(rarefy_all_samples(1, 10), "data_to_harmonize")
  expect_error(rarefy_all_samples("a", 10), "data_to_harmonize")
  expect_error(rarefy_all_samples(list(), 10), "data_to_harmonize")
})

test_that("rarefy_all_samples() validates required columns", {
  df <- data.frame(a = 1, b = 2)
  expect_error(rarefy_all_samples(df, 10), "dataset_id")
})

test_that("rarefy_all_samples() handles empty data.frame", {
  df <- data.frame(dataset_id = character(), age = numeric())
  res <- rarefy_all_samples(df, 10)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("rarefy_all_samples() works with minimal valid data", {
  df <- data.frame(
    dataset_id = c("d1", "d2"),
    age = c(100, 200),
    sp1 = c(5, 2),
    sp2 = c(3, 0)
  )
  
  set.seed(42)
  res <- rarefy_all_samples(df, n_grains = 4)
  
  expect_s3_class(res, "data.frame")
  expect_true(all(c("dataset_id_age", "sp1", "sp2", "dataset_id", "age") %in% colnames(res)))
  expect_equal(nrow(res), nrow(df))
  expect_equal(res$dataset_id, df$dataset_id)
  expect_equal(as.numeric(res$age), df$age)
})

test_that("rarefy_all_samples() replaces NAs with zero", {
  df <- data.frame(
    dataset_id = "d1",
    age = 100,
    sp1 = NA,
    sp2 = 2
  )
  res <- rarefy_all_samples(df, n_grains = 2)
  expect_equal(res$sp1, 0)
  expect_equal(res$sp2, 2)
})

test_that("rarefy_all_samples() preserves row count after rarefaction", {
  df <- data.frame(
    dataset_id = c("d1", "d2"),
    age = c(100, 200),
    sp1 = c(5, 5),
    sp2 = c(5, 5)
  )
  res <- rarefy_all_samples(df, n_grains = 5)
  expect_equal(nrow(res), 2)
})

test_that("rarefy_all_samples() errors if n_grains exceeds total counts", {
  df <- data.frame(
    dataset_id = "d1",
    age = 100,
    sp1 = 2,
    sp2 = 1
  )
  expect_error(rarefy_all_samples(df, n_grains = 10))
})

test_that("rarefy_all_samples() works wi_
