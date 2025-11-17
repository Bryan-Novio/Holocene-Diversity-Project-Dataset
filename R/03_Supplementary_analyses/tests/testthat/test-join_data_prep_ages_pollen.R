library(testthat)
library(dplyr)

test_that("join_data_prep_ages_pollen() validates input types", {
  neotoma_taxa <<- data.frame(taxon_name = "A", neotoma_names = "NeoA")
  
  expect_error(join_data_prep_ages_pollen(NULL), "data_prepared")
  expect_error(join_data_prep_ages_pollen(123), "data_prepared")
  expect_error(join_data_prep_ages_pollen("text"), "data_prepared")
})

test_that("join_data_prep_ages_pollen() errors when required columns are missing", {
  neotoma_taxa <<- data.frame(taxon_name = "A", neotoma_names = "NeoA")
  
  df_missing_taxa <- data.frame(x = 1)
  expect_error(join_data_prep_ages_pollen(df_missing_taxa))
  
  df_missing_join <- data.frame(taxa = "A")
  expect_error(join_data_prep_ages_pollen(df_missing_join))
})

test_that("join_data_prep_ages_pollen() returns correctly structured output", {
  neotoma_taxa <<- data.frame(
    taxon_name = c("A", "B"),
    neotoma_names = c("NeoA", "NeoB")
  )
  
  data_prepared <- data.frame(
    dataset_id = c(1, 2),
    age = c(100, 200),
    pollen_counts = c(10, 20),
    sample_id = c("s1", "s2"),
    taxa = c("A", "B")
  )
  
  out <- join_data_prep_ages_pollen(data_prepared)
  
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_named(out, c("dataset_id", "age", "pollen_counts", "sample_id", "taxon_name"))
})

test_that("join_data_prep_ages_pollen() joins correctly and renames taxa", {
  neotoma_taxa <<- data.frame(
    taxon_name = "A",
    neotoma_names = "NeoA"
  )
  
  data_prepared <- data.frame(
    dataset_id = 1,
    age = 100,
    pollen_counts = 10,
    sample_id = "s1",
    taxa = "A"
  )
  
  out <- join_data_prep_ages_pollen(data_prepared)
  
  expected <- data.frame(
    dataset_id = 1,
    age = 100,
    pollen_counts = 10,
    sample_id = "s1",
    taxon_name = "NeoA"
  )
  
  expect_equal(out, expected)
})

test_that("join_data_prep_ages_pollen() drops rows without matching taxa", {
  neotoma_taxa <<- data.frame(
    taxon_name = "A",
    neotoma_names = "NeoA"
  )
  
  data_prepared <- data.frame(
    dataset_id = c(1, 2),
    age = c(100, 200),
    pollen_counts = c(10, 20),
    sample_id = c("s1", "s2"),
    taxa = c("A", "B")
  )
  
  out <- join_data_prep_ages_pollen(data_prepared)
  
  expect_equal(nrow(out), 1)
  expect_equal(out$dataset_id, 1)
})
