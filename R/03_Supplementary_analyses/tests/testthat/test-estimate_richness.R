#----------------------------------------------------------#
#
#                 ----  UNIT TESTING  ----
#
#                   estimate_richness()
#
#----------------------------------------------------------#

library(testthat)
library(tidyverse)

#----------------------------------------------------------#
# 1. Test 1: Input Type Validation -------------------------
#----------------------------------------------------------#


test_that("estimate_richness() validates input types", {
  # All inputs must be a data frame
  expect_error(estimate_richness(NULL), "data.frame")
  expect_error(estimate_richness(1), "data.frame")
  expect_error(estimate_richness("a"), "data.frame")
  expect_error(estimate_richness(list()), "data.frame")
})

#----------------------------------------------------------#
# 2. Test 2: Required Columns Validation -------------------
#----------------------------------------------------------#

test_that("estimate_richness() requires required columns", {
  # Use tibble::tibble for convention
  df_missing_cols <- tibble::tibble(a = 1, b = 2)
  expect_error(estimate_richness(df_missing_cols))
  
  df_missing_age <- tibble::tibble(
    dataset_id = 1,
    avg_n_pollen_grains = 2
  )
  expect_error(estimate_richness(df_missing_age))
  
  df_missing_dataset <- tibble::tibble(
    age = 1000,
    avg_n_pollen_grains = 5
  )
  expect_error(estimate_richness(df_missing_dataset))
  
  df_missing_pollen <- tibble::tibble(
    dataset_id = 1,
    age = 1000
  )
  expect_error(estimate_richness(df_missing_pollen))
})


#----------------------------------------------------------#
# 3. Test 3: Works on Valid Input & Output Structure -------
#----------------------------------------------------------#

test_that("estimate_richness() works on valid input", {
  df <- tibble::tibble( 
    dataset_id = c(1, 1, 1, 2, 2),
    age = c(100, 100, 200, 100, 200),
    avg_n_pollen_grains = c(1, 0, 3, 2, 0)
  )
  

  expect_no_error(result <- df |> estimate_richness())
  
  # Assert result class is a tibble, which inherits from data.frame
  expect_s3_class(result, "data.frame") 
  expect_s3_class(result, "tbl_df") 
  
  # The column names must match
  expect_named(result, c("dataset_id", "age", "richness"))
})

#----------------------------------------------------------#
# 4. Test 4: Correct Richness Values (Core Logic)  -------
#----------------------------------------------------------#

test_that("estimate_richness() computes correct richness values", {
  df <- tibble::tibble(
    dataset_id = c(1, 1, 1, 2, 2),
    age = c(100, 100, 200, 100, 200),
    avg_n_pollen_grains = c(1, 0, 3, 2, 0)
  )
  
  expected <- tibble::tibble(
    dataset_id = c(1, 1, 2, 2),
    age = c(100, 200, 100, 200),
    richness = c(1, 1, 1, 0)
  )
  
  result <- df %>% 
    estimate_richness() %>% 
    dplyr::arrange(dataset_id, age)
  
  expected <- expected %>% 
    dplyr::arrange(dataset_id, age)
  expect_equivalent(result, expected) 
})
#----------------------------------------------------------#
# 5. Test 5: Handling NA Values
#----------------------------------------------------------#

test_that("estimate_richness() handles NA values in avg_n_pollen_grains", {
  df <- tibble::tibble(
    dataset_id = c(1, 1, 1),
    age = c(100, 100, 100),
    avg_n_pollen_grains = c(1, NA, 0)
  )
  
  expected <- tibble::tibble(
    dataset_id = 1,
    age = 100,
    richness = 1
  )
  
  result <- df |> estimate_richness()
  
  expect_equivalent(result, expected)
})

#----------------------------------------------------------#
# 6.  Test 6: Zero-Row Data Frame (Empty Input)
#----------------------------------------------------------#

test_that("estimate_richness() handles zero-row data frame", {
  df <- tibble::tibble( 
    dataset_id = numeric(0),
    age = numeric(0),
    avg_n_pollen_grains = numeric(0)
  )
  
  result <- df |> estimate_richness()
  
  expect_s3_class(result, "tbl_df") 
  expect_equal(nrow(result), 0)
  expect_named(result, c("dataset_id", "age", "richness"))
})

#----------------------------------------------------------#
# 7.  Test 7: Negative or Extreme Values
#----------------------------------------------------------#

test_that("estimate_richness() handles negative or extreme values", {
  df <- tibble::tibble(
    dataset_id = c(1, 1),
    age = c(100, 100),
    avg_n_pollen_grains = c(-5, 10)
  )
  
  expected <- tibble::tibble(
    dataset_id = 1,
    age = 100,
    richness = 1
  )
  
  result <- df |> estimate_richness()
  expect_equivalent(result, expected)
})







