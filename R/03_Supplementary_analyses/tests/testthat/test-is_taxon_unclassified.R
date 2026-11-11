#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("is_taxon_unclassified() accepts valid inputs", {
  list_input <-
    base::list(
      result = data.frame(
        taxon = c("A", "B"),
        classification = c("X", "Y")
      ),
      error = NULL
    )

  testthat::expect_no_error(
    res <-
      is_taxon_unclassified(x = list_input)
  )

  testthat::expect_false(res)
})

testthat::test_that("is_taxon_unclassified() validates x is a list", {
  list_bad_x <-
    base::list(
      NULL,
      "result",
      1,
      TRUE,
      base::as.environment(base::list(result = NULL))
    )

  for (
    sel_bad in list_bad_x
  ) {
    testthat::expect_error(
      is_taxon_unclassified(x = sel_bad),
      "`x` must be a list\\."
    )
  }
})

testthat::test_that("is_taxon_unclassified() requires a result element", {
  list_no_result <-
    base::list(
      base::list(),
      base::list(error = NULL),
      base::list(data.frame(classification = "X")),
      base::list(results = data.frame(classification = "X"))
    )

  for (
    sel_input in list_no_result
  ) {
    testthat::expect_error(
      is_taxon_unclassified(x = sel_input),
      "`x` must contain an element named 'result'\\."
    )
  }
})

testthat::test_that("is_taxon_unclassified() validates result type", {
  list_bad_result <-
    base::list(
      "classification",
      1:3,
      TRUE,
      base::list(classification = "X"),
      c(classification = "X")
    )

  for (
    sel_bad in list_bad_result
  ) {
    testthat::expect_error(
      is_taxon_unclassified(
        x = base::list(result = sel_bad)
      ),
      "The 'result' element of `x` must be NULL or a data frame\\."
    )
  }
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("is_taxon_unclassified() returns one logical", {
  data_with <-
    data.frame(classification = c("abies", "alnus"))

  data_without <-
    data.frame(taxon = c("abies", "alnus"))

  list_inputs <-
    base::list(
      base::list(result = data_with),
      base::list(result = data_without),
      base::list(result = NULL)
    )

  for (
    sel_input in list_inputs
  ) {
    res <-
      is_taxon_unclassified(x = sel_input)

    testthat::expect_type(res, "logical")
    testthat::expect_length(res, 1L)
    testthat::expect_false(base::is.na(res))
  }
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("is_taxon_unclassified() is FALSE if column exists", {
  data_result <-
    data.frame(
      taxon = c("A", "B"),
      classification = c("X", "Y")
    )

  testthat::expect_false(
    is_taxon_unclassified(
      x = base::list(result = data_result)
    )
  )

  data_only <-
    data.frame(classification = c("abies", "alnus", "acer"))

  testthat::expect_false(
    is_taxon_unclassified(
      x = base::list(result = data_only)
    )
  )
})

testthat::test_that("is_taxon_unclassified() is TRUE if column missing", {
  data_result <-
    data.frame(
      value1 = 1:2,
      value2 = 3:4
    )

  testthat::expect_true(
    is_taxon_unclassified(
      x = base::list(result = data_result)
    )
  )

  data_taxon <-
    data.frame(taxon = c("A", "B"))

  testthat::expect_true(
    is_taxon_unclassified(
      x = base::list(result = data_taxon)
    )
  )
})

testthat::test_that("is_taxon_unclassified() is TRUE for NULL result", {
  testthat::expect_true(
    is_taxon_unclassified(
      x = base::list(result = NULL)
    )
  )

  testthat::expect_true(
    is_taxon_unclassified(
      x = base::list(result = NULL, error = "failed")
    )
  )
})

testthat::test_that("is_taxon_unclassified() ignores the error element", {
  data_result <-
    data.frame(classification = "X")

  testthat::expect_false(
    is_taxon_unclassified(
      x = base::list(result = data_result, error = "some error")
    )
  )
})

testthat::test_that("is_taxon_unclassified() checks the name only", {
  data_na <-
    data.frame(classification = NA)

  testthat::expect_false(
    is_taxon_unclassified(
      x = base::list(result = data_na)
    )
  )

  data_other_case <-
    data.frame(Classification = "X")

  testthat::expect_true(
    is_taxon_unclassified(
      x = base::list(result = data_other_case)
    )
  )

  data_partial <-
    data.frame(classification_id = 1L)

  testthat::expect_true(
    is_taxon_unclassified(
      x = base::list(result = data_partial)
    )
  )
})

testthat::test_that("is_taxon_unclassified() works with safely output", {
  fun_ok <-
    purrr::safely(.f = function(x) data.frame(classification = x))

  fun_fail <-
    purrr::safely(.f = function(x) base::stop("classification failed"))

  list_ok <-
    fun_ok("Abies")

  list_fail <-
    fun_fail("Abies")

  testthat::expect_false(
    is_taxon_unclassified(x = list_ok)
  )
  testthat::expect_true(
    is_taxon_unclassified(x = list_fail)
  )

  vec_flags <-
    purrr::map_lgl(
      .x = base::list(list_ok, list_fail, list_ok),
      .f = is_taxon_unclassified
    )

  testthat::expect_equal(vec_flags, c(FALSE, TRUE, FALSE))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("is_taxon_unclassified() handles empty results", {
  testthat::expect_true(
    is_taxon_unclassified(
      x = base::list(result = data.frame())
    )
  )

  data_zero_rows <-
    data.frame(classification = character(0))

  testthat::expect_false(
    is_taxon_unclassified(
      x = base::list(result = data_zero_rows)
    )
  )
})

testthat::test_that("is_taxon_unclassified() accepts a tibble result", {
  data_tibble <-
    tibble::tibble(
      taxon = "A",
      classification = "X"
    )

  testthat::expect_false(
    is_taxon_unclassified(
      x = base::list(result = data_tibble)
    )
  )

  testthat::expect_true(
    is_taxon_unclassified(
      x = base::list(result = tibble::tibble(taxon = "A"))
    )
  )
})
