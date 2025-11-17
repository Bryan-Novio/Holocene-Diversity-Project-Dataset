library(testthat)
library(dplyr)

test_that("harmonize_taxa() validates input classes", {
  expect_error(
    harmonize_taxa(NULL, data.frame(), "level"),
    "data_to_harmonize has to be data.frame"
  )
  
  good_data <- data.frame(
    dataset_id = 1,
    age = 100,
    taxon_name = "A",
    pollen_counts = 5
  )
  
  expect_error(
    harmonize_taxa(good_data, NULL, "level"),
    "object"
  )
  
  expect_error(
    harmonize_taxa(good_data, data.frame(), NA),
    "unused argument|object"
  )
})

test_that("harmonize_taxa() requires specific columns in data_to_harmonize", {
  bad <- data.frame(x = 1:3)
  expect_error(
    harmonize_taxa(bad, data.frame(taxon_name = "A", level = "B"), "level"),
    "data must contain columns"
  )
})

test_that("harmonize_taxa() errors when harmonisation table lacks taxon names", {
  df <- data.frame(
    dataset_id = c(1,1),
    age = c(10,10),
    taxon_name = c("A","B"),
    pollen_counts = c(2,3)
  )
  
  harm <- data.frame(
    taxon_name = c("A"),
    lvl = c("X")
  )
  
  expect_error(
    harmonize_taxa(df, harm, "lvl"),
    "harmonisation table must contain taxon_names"
  )
})

test_that("harmonize_taxa() happy path returns a data frame", {
  df <- data.frame(
    dataset_id = c(1,1,1),
    age = c(10,10,10),
    taxon_name = c("A","A","B"),
    pollen_counts = c(2,3,5)
  )
  
  harm <- data.frame(
    taxon_name = c("A","B"),
    lvl = c("X","Y")
  )
  
  res <- harmonize_taxa(df, harm, "lvl")
  expect_s3_class(res, "data.frame")
  expect_named(res, c("dataset_id","age","taxon_name","pollen_counts"))
})

test_that("harmonize_taxa() aggregates pollen counts correctly", {
  df <- data.frame(
    dataset_id = c(1,1,1,1),
    age = c(10,10,10,10),
    taxon_name = c("A","A","B","B"),
    pollen_counts = c(1,2,3,4)
  )
  
  harm <- data.frame(
    taxon_name = c("A","B"),
    lvl = c("X","Y")
  )
  
  res <- harmonize_taxa(df, harm, "lvl")
  
  expect_equal(
    res[res$taxon_name == "X", "pollen_counts"],
    3
  )
  
  expect_equal(
    res[res$taxon_name == "Y", "pollen_counts"],
    7
  )
})

test_that("harmonize_taxa() handles duplicated harmonisation table entries", {
  df <- data.frame(
    dataset_id = c(1,1),
    age = c(10,10),
    taxon_name = c("A","B"),
    pollen_counts = c(5,7)
  )
  
  harm <- data.frame(
    taxon_name = c("A","A","B"),
    lvl = c("X","X","Y")
  )
  
  res <- harmonize_taxa(df, harm, "lvl")
  
  expect_equal(
    res[res$taxon_name == "X", "pollen_counts"],
    5
  )
  
  expect_equal(
    res[res$taxon_name == "Y", "pollen_counts"],
    7
  )
})

test_that("harmonize_taxa() works with single-row input", {
  df <- data.frame(
    dataset_id = 1,
    age = 10,
    taxon_name = "A",
    pollen_counts = 5
  )
  
  harm <- data.frame(
    taxon_name = "A",
    lvl = "X"
  )
  
  res <- harmonize_taxa(df, harm, "lvl")
  
  expect_equal(nrow(res), 1)
  expect_equal(res$pollen_counts, 5)
  expect_equal(res$taxon_name, "X")
})
