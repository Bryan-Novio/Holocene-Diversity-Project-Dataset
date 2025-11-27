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
  expect_error(rarefy_all_samples(df, 10), "meaningful only for integers")
})

test_that("rarefy_all_samples() works with minimal valid data", {
  df <- data.frame(
    dataset_id = c("1001", "1002"),
    age = c(100, 200),
    abies = c(5, 2), 
    alnus = c(3, 0)
  )
  
  set.seed(42)
  expect_warning({
    res <- rarefy_all_samples(df, n_grains = 4)
  }, "observed counts|row sums < 'sample'") 
  
  expect_s3_class(res, "data.frame")
  expect_true(all(c("dataset_id_age", "abies", "alnus") %in% colnames(res)))
  expect_equal(nrow(res), nrow(df))
})

test_that("rarefy_all_samples() replaces NAs with zero", {
  df <- data.frame(
    dataset_id = "1001",
    age = 100,
    abies = NA, 
    alnus = 2  
  )

  expect_warning({
    res <- rarefy_all_samples(df, n_grains = 2)
  }, "function should be used for observed counts")
  
  expect_equal(res$abies, 0)
  expect_equal(res$alnus, 2)
})

test_that("rarefy_all_samples() preserves row count after rarefaction", {
  df <- data.frame(
    dataset_id = c("1001", "1002"),
    age = c(100, 200),
    abies = c(5, 5),
    alnus = c(5, 5) 
  )

  expect_warning({
    res <- rarefy_all_samples(df, n_grains = 5)
  }, "function should be used for observed counts")
  
  expect_equal(nrow(res), 2)
})

test_that("rarefy_all_samples() errors if n_grains exceeds total counts", {
  df <- data.frame(
    dataset_id = "1001",
    age = 100,
    abies = 2,
    alnus = 1 
  )

  expect_warning({
    rarefy_all_samples(df, n_grains = 10)
  }, "some row sums < 'sample' and are not rarefied")
})

