library(testthat)
library(dplyr)
library(tidyr)

test_that("get_pollen_counts() accepts valid input and returns expected structure", {
  raw1 <- tibble(sample_id = c("s1","s2"), TaxonA = c(10, 20), TaxonB = c(5, 0))
  raw2 <- tibble(sample_id = c("s3"), TaxonA = 3, TaxonB = 7)
  data_compilation <- tibble(
    dataset_id = c("d1","d2"),
    raw_counts = list(raw1, raw2)
  )
  
  res <- get_pollen_counts(data_compilation)
  
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("dataset_id","sample_id","taxa","pollen_counts") %in% names(res)))
  expect_type(res$pollen_counts, "integer")
  expect_equal(nrow(res), 6)
})

test_that("get_pollen_counts() produces correct long-format counts", {
  raw <- tibble(sample_id = c("s1","s2"), A = c(1,2), B = c(3,4))
  data <- tibble(dataset_id = "dX", raw_counts = list(raw))
  
  res <- get_pollen_counts(data)
  
  expected <- tibble(
    dataset_id = rep("dX", 4),
    sample_id = rep(c("s1","s2"), each = 2),
    taxa = rep(c("A","B"), 2),
    pollen_counts = c(1,3,2,4)
  )
  
  expect_equal(
    arrange(res, sample_id, taxa),
    arrange(expected, sample_id, taxa)
  )
})

test_that("get_pollen_counts() handles zero-row input", {
  data_empty <- tibble(dataset_id = character(), raw_counts = list())
  
  expect_error(get_pollen_counts(data_empty), regexp = ".*")
})

test_that("get_pollen_counts() errors on incorrect input types", {
  expect_error(get_pollen_counts(NULL), regexp = ".*")
  expect_error(get_pollen_counts(list(a = 1)), regexp = ".*")
  expect_error(get_pollen_counts(data.frame(x = 1)), regexp = ".*")
})

test_that("get_pollen_counts() errors when required columns are missing", {
  bad <- tibble(id = 1, raw_counts = list(tibble(sample_id = "s", A = 1)))
  expect_error(get_pollen_counts(bad), regexp = "dataset_id")
  
  bad2 <- tibble(dataset_id = "d", rc = list(tibble(sample_id = "s", A = 1)))
  expect_error(get_pollen_counts(bad2), regexp = "raw_counts")
})

test_that("get_pollen_counts() errors when raw_counts is not a list-column of data frames", {
  bad <- tibble(dataset_id = "d", raw_counts = 5)
  expect_error(get_pollen_counts(bad), regexp = ".*")
  
  bad2 <- tibble(dataset_id = "d", raw_counts = list(5))
  expect_error(get_pollen_counts(bad2), regexp = ".*")
})

test_that("get_pollen_counts() errors when raw_counts elements lack sample_id", {
  bad_raw <- tibble(A = 1, B = 2)
  data <- tibble(dataset_id = "d", raw_counts = list(bad_raw))
  expect_error(get_pollen_counts(data), regexp = "sample_id")
})
