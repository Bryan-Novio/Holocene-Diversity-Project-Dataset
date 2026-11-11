
return_taxa_not_classified <- function(x) {   # function to id taxon with failed classifications

  name_cols <- colnames(x$result) 

  is_classification_present <- "classification" %in% name_cols

  return(!is_classification_present)
  
  # Test cases
  test_cases <- list(
    list(input = list(result = data.frame(taxon = c("A", "B"), classification = c("X", "Y"))),
         expected = FALSE),
    list(input = list(result = data.frame(taxon = c("A", "B"))),
         expected = TRUE)
  )
  
  # Assertion for all test cases
  for(tc in test_cases) {
    assert_that(is.logical(return_taxa_not_classified(tc$input)))
    assert_that(return_taxa_not_classified(tc$input) == tc$expected)
  }

}

