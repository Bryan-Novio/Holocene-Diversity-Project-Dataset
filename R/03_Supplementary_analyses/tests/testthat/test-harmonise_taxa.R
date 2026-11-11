library(testthat)
library(tidyverse)

test_that("harmonize_taxa() validates input classes", {
  expect_error(
    harmonize_taxa(NULL, data.frame(), "level"),
    "data_to_harmonize has to be data.frame"
  )
  
  data_clean <- data.frame(
    dataset_id = 1001,
    age = 100,
    taxon_name = "abies",
    pollen_counts = 5
  )
  
  expect_error(
    harmonize_taxa(data_clean, NULL, "level"),
    "object"
  )
  
  expect_error(
    harmonize_taxa(data_clean, data.frame(), NA),
    "convert|symbol"
  )
})

test_that("harmonize_taxa() requires specific columns in data_to_harmonize", {
  data_unfix <- data.frame(x = 1:3)
  expect_error(
    harmonize_taxa(data_unfix, data.frame(taxon_name = "A", level = "B"), "level"),
    "data must contain columns"
  )
})

test_that("harmonize_taxa() errors when harmonisation table lacks taxon names", {
  df <- data.frame(
    dataset_id = c(15081,15081),
    age = c(10,10),
    taxon_name = c("abies","alnus"),
    pollen_counts = c(2,3)
  )
  
  harm <- data.frame(
    taxon_name = c("abies"),
    level = c("level_5")
  )
  
  expect_error(
    harmonize_taxa(df, harm, "level_5")
  )
})


test_that("harmonize_taxa() correct path returns a data frame", {
  df <- data.frame(
    dataset_id = c(1001,1001,1001),
    age = c(10,10,10),
    taxon_name = c("abies","abies","alnus"),
    pollen_counts = c(2,3,5)
  )
  
  harm <- data.frame(
    taxon_name = c("abies","alnus"),
    level = c("level_5","level_6")
  )
  
  res <- harmonize_taxa(df, harm, "level")
  expect_s3_class(res, "data.frame")
  expect_named(res, c("dataset_id","age","taxon_name","pollen_counts"))
})

test_that("harmonize_taxa() aggregates pollen counts correctly", {
  
  df <- data.frame(
    dataset_id   = c(1001,1001,1001,1001),
    age          = c(10,10,10,10),
    taxon_name   = c("abies","abies","alnus","alnus"),
    pollen_counts = c(0,2,6,90)
  )
  
  harm <- data.frame(
    taxon_name = c("abies","alnus"),
    level = c("abies","alnus")   
  )
  
  expected <- data.frame(
    dataset_id   = c(1001, 1001),
    age          = c(10, 10),
    taxon_name   = c("abies", "alnus"),
    pollen_counts = c(2, 96)
  )
  
  result <- harmonize_taxa(df, harm, "level")
  

  result_df <- as.data.frame(result)
  
  expect_equal(
    result_df[order(result_df$taxon_name), ],
    expected[order(expected$taxon_name), ]
  )
})

test_that("harmonize_taxa() handles duplicated harmonisation table entries", {
  
  df <- data.frame(
    dataset_id   = c(1001,1001),
    age          = c(10,10),
    taxon_name   = c("abies","alnus"),
    pollen_counts = c(5,7)
  )
  
  harm <- data.frame(
    taxon_name = c("abies","abies","alnus"),
    level      = c("abies","abies","alnus")
  )
  
  expect_no_error(res <- harmonize_taxa(df, harm, "level"))
  
  res <- as.data.frame(res)   
  
  expect_true(is.data.frame(res))
  
  expect_equal(
    res$pollen_counts[res$taxon_name == "abies"],
    5
  )

  expect_equal(
    res$pollen_counts[res$taxon_name == "alnus"],
    7
  )
})


test_that("harmonize_taxa() works with single-row input", {
  df <- data.frame(
    dataset_id = 1001,
    age = 10,
    taxon_name = "abies",
    pollen_counts = 5
  )
  
  harm <- data.frame(
    taxon_name = "abies",
    level = "level_5"
  )
  
  res <- harmonize_taxa(df, harm, "level")
  
  expect_equal(nrow(res), 1)
  expect_equal(res$pollen_counts, 5)
  expect_equal(res$taxon_name, "level_5")
})

