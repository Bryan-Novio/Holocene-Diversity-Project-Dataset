library(testthat)
library(tidyverse)
library(assertthat)

test_that("join_data_prep_ages_pollen() errors for invalid input types", {
  neotoma_taxa <<- data.frame(taxon_name = "abies", neotoma_names = "Abies")
  
  expect_error(join_data_prep_ages_pollen(NULL))
  expect_error(join_data_prep_ages_pollen(123))
  expect_error(join_data_prep_ages_pollen("text"))
})

test_that("join_data_prep_ages_pollen() errors when required columns are missing", {
  neotoma_taxa <<- data.frame(taxon_name = "abies", neotoma_names = "Abies")
  
  df_missing_taxa <- data.frame(x = 1)
  expect_error(join_data_prep_ages_pollen(df_missing_taxa))
  
  df_missing_join <- data.frame(taxa = "abies")
  expect_error(join_data_prep_ages_pollen(df_missing_join))
})

test_that("join_data_prep_ages_pollen() returns correctly structured output", {
  neotoma_taxa <<- data.frame(
    taxon_name = c("abies", "alnus"),
    neotoma_names = c("Abies", "Alnus")
  )
  
  data_prepared <- data.frame(
    dataset_id = c(1001, 1002),
    age = c(100, 200),
    pollen_counts = c(10, 20),
    sample_id = c("21011", "21012"),
    taxa = c("abies", "alnus"),
    stringsAsFactors = FALSE
  )
  
  out <- join_data_prep_ages_pollen(data_prepared)
  
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_named(
    out,
    c("dataset_id", "age", "pollen_counts", "sample_id", "taxon_name"),
    ignore.order = FALSE
  )
})


test_that("join_data_prep_ages_pollen() joins correctly and renames taxa", {
  neotoma_taxa <<- data.frame(
    taxon_name = "abies",
    neotoma_names = "Abies"
  )
  
  data_prepared <- data.frame(
    dataset_id = 1,
    age = 100,
    pollen_counts = 10,
    sample_id = "21011",
    taxa = "abies"
  )
  
  out <- join_data_prep_ages_pollen(data_prepared)
  
  expected <- data.frame(
    dataset_id = 1,
    age = 100,
    pollen_counts = 10,
    sample_id = "21011",
    taxon_name = "Abies"
  )
  
  expect_equal(out, expected)
})

test_that("join_data_prep_ages_pollen() drops rows without matching taxa", {
  neotoma_taxa <<- data.frame(
    taxon_name = "abies",
    neotoma_names = "Abies"
  )
  
  data_prepared <- data.frame(
    dataset_id = c(1, 2),
    age = c(100, 200),
    pollen_counts = c(10, 20),
    sample_id = c("21011", "21012"),
    taxa = c("abies", "alnus")
  )
  
  out <- join_data_prep_ages_pollen(data_prepared)
  
  expect_equal(nrow(out), 1)
  expect_equal(out$dataset_id, 1)
})

