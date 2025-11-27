library(testthat)
library(dplyr)
library(assertthat)

### helper function 
compare_dfs <- function(result, expected) {
  
  result_cleaned <- result %>%
    dplyr::ungroup() %>%
    as.data.frame()
  
  expected_cleaned <- expected %>% 
    as.data.frame() 
  
  result_cleaned <- result_cleaned[order(result_cleaned$dataset_id, result_cleaned$age), ]
  expected_cleaned <- expected_cleaned[order(expected_cleaned$dataset_id, expected_cleaned$age), ]
  
  rownames(result_cleaned) <- NULL
  rownames(expected_cleaned) <- NULL
  
  expect_equal(result_cleaned, expected_cleaned, check.attributes = FALSE) 
}

test_that("estimate_richness() fails gracefully on invalid input types", {
  
  expect_error(estimate_richness(list()), 
               "data_for_richness_estimation has to be a data.frame")
  
  df_missing_dataset_id <- data.frame(age = c(100), pollen_grains = c(5))
  
  expect_error(
    estimate_richness(df_missing_dataset_id), 
    "Column `dataset_id` is not found.",
    fixed = TRUE
  )
})

test_that("estimate_richness() works on valid input and returns a data.frame", {

  df <- data.frame(
    dataset_id = c("15081", "15081", "1541", "1541", "16111", "16111"),
    age = c(100, 200, 100, 200, 100, 300),
    pollen_grains = c(10, 0, 5, 20, 1, 0),
    species = c("abies", "alnus", "acer", "amaranthanceae", "pinus", "picea") 
  )
  
  result <- estimate_richness(df) 
  
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("dataset_id", "age", "richness"))
  expect_true(nrow(result) > 0)
})

test_that("estimate_richness() computes correct richness values", {
 
  df <- data.frame(
    dataset_id = c(rep("15081", 3), rep("1541", 3), rep("16111", 2)),
    age = c(100, 100, 200, 200, 200, 100, 100, 200),
    pollen_grains = c(10, 0, 1, 5, 20, 0, 0, 100) 
  )
  
  expected <- data.frame(
    dataset_id = c("15081", "15081", "1541", "1541", "16111", "16111"),
    age = c(100, 200, 100, 200, 100, 200),
    richness = c(1, 1, 0, 2, 0, 1)
  )
  
  result <- estimate_richness(df)

  compare_dfs(result, expected) 
})


test_that("estimate_richness() handles NA values in pollen_grains column", {

  df <- data.frame(
    dataset_id = c("15081", "15081", "1541", "1541", "16111"),
    age = c(100, 200, 100, 200, 100),
    pollen_grains = c(10, NA, 5, 0, NA) 
  )
  
  expected <- data.frame(
    dataset_id = c("15081", "15081", "1541", "1541", "16111"),
    age = c(100, 200, 100, 200, 100),
    richness = c(1, 0, 1, 0, 0)
  )
  
  result <- estimate_richness(df)
  compare_dfs(result, expected)
})


test_that("estimate_richness() handles negative or extreme values", {
  
  df <- data.frame(
    dataset_id = c("15081", "15081", "1541", "1541"),
    age = c(10, 20, 10, 20),
    pollen_grains = c(-5, 0.5, 99999, 1) 
  )
  
  expected <- data.frame(
    dataset_id = c("15081", "15081", "1541", "1541"), 
    age = c(10, 20, 10, 20),           
    richness = c(0, 0, 1, 1)           
  )
  
  result <- estimate_richness(df)
  compare_dfs(result, expected)
})