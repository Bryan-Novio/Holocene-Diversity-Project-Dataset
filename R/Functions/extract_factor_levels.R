extract_factor_levels <- function(model, sel_term) {
  require(dplyr)
  require(stringr)
  require(assertthat)
  require(broom)

  assertthat::assert_that(
    inherits(
      model,
      c("gam", "bam")
    ),
    msg = "`model` must be a GAM/BAM object."
  )

  assertthat::assert_that(
    length(sel_term) == 1,
    is.character(sel_term),
    msg = "`sel_term` must be a single character string."
  )

  vec_levels <-
    model |>
    broom::tidy() |>
    dplyr::filter(
      stringr::str_detect(
        term,
        paste0(":", sel_term)
      )
    ) |>
    dplyr::mutate(
      level = stringr::str_extract(
        term,
        paste0(":", sel_term, "(.*)")
      ) |>
        stringr::str_remove(
          paste0(":", sel_term)
        )
    ) |>
    dplyr::pull(level)

  assertthat::assert_that(
    length(vec_levels) >= 1,
    msg = paste0(
      "No factor levels found for term '",
      sel_term,
      "'."
    )
  )

  return(vec_levels)
}
