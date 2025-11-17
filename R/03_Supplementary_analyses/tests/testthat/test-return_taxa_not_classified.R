library(testthat)

test_that("return_taxa_not_classified() validates input types", {
  expect_error(return_taxa_not_classified(1:5), "must be a list with element 'result'")
  expect_error(return_taxa_not_classified(list()), "must contain element 'result'")
  expect_error(return_taxa_not_classified(list(result = 5)), "result must be a data.frame or matrix")
  expect_error(return_taxa_not_classified(NULL), "must be a list with element 'result'")
})

test_that("return_taxa_not_classified() handles empty data.frames correctly", {
  empty_df <- data.frame()
  expect_equal(return_taxa_not_classified(list(result = empty_df)), TRUE)
})

test_that("return_taxa_not_classified() returns TRUE if 'classification' column is missing", {
  df <- data.frame(other_col = 1:3)
  result <- return_taxa_not_classified(list(result = df))
  expect_type(result, "logical")
  expect_length(result, 1)
  expect_equal(result, TRUE)
})

test_that("return_taxa_not_classified() returns FALSE if 'classification' column is present", {
  df <- data.frame(classification = c("A", "B", "C"))
  result <- return_taxa_not_classified(list(result = df))
  expect_type(result, "logical")
  expect_length(result, 1)
  expect_equal(result, FALSE)
})

test_that("return_taxa_not_classified() works with multiple columns including 'classification'", {
  df <- data.frame(classification = c("X", "Y"), value = c(10, 20))
  result <- return_taxa_not_classified(list(result = df))
  expect_equal(result, FALSE)
})

test_that("return_taxa_not_classified() works with multiple columns excluding 'classification'", {
  df <- data.frame(value1 = 1:2, value2 = 3:4)
  result <- return_taxa_not_classified(list(result = df))
  expect_equal(result, TRUE)
})

test_that("return_taxa_not_classified() handles NA and unusual column names", {
  df <- data.frame(`NA` = 1:2, value = 3:4)
  expect_equal(return_taxa_not_classified(list(result = df)), TRUE)
  
  df2 <- data.frame(classification = NA)
  expect_equal(return_taxa_not_classified(list(result = df2)), FALSE)
})

test_that("return_taxa_not_classified() returns a single logical scalar", {
  df <- data.frame(a = 1:3)
  result <- return_taxa_not_classified(list(result = df))
  expect_type(result, "logical")
  expect_length(result, 1)
})
