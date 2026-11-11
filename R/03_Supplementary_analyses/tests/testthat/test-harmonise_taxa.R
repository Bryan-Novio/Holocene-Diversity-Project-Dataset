#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("harmonise_taxa() accepts valid inputs", {
  data_input <-
    tibble::tibble(
      dataset_id = c(1, 1),
      age = c(100, 100),
      taxon_name = c("Abies alba", "Alnus glutinosa"),
      pollen_counts = c(2, 3)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = c("Abies alba", "Alnus glutinosa"),
      genus = c("Abies", "Alnus")
    )

  testthat::expect_no_error(
    res <-
      harmonise_taxa(
        data_to_harmonize = data_input,
        harmonisation_table = data_harm,
        level = "genus"
      )
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(base::nrow(res), 2L)
})

testthat::test_that("harmonise_taxa() rejects non-data-frame data", {
  data_harm <-
    tibble::tibble(
      taxon_name = "Abies alba",
      genus = "Abies"
    )

  testthat::expect_error(
    harmonise_taxa(
      data_to_harmonize = NULL,
      harmonisation_table = data_harm,
      level = "genus"
    ),
    "data_to_harmonize has to be data.frame"
  )

  testthat::expect_error(
    harmonise_taxa(
      data_to_harmonize = base::list(taxon_name = "Abies alba"),
      harmonisation_table = data_harm,
      level = "genus"
    ),
    "data_to_harmonize has to be data.frame"
  )
})

testthat::test_that("harmonise_taxa() validates required data columns", {
  data_harm <-
    tibble::tibble(
      taxon_name = "Abies alba",
      genus = "Abies"
    )

  vec_required <-
    c("dataset_id", "age", "taxon_name", "pollen_counts")

  data_full <-
    tibble::tibble(
      dataset_id = 1,
      age = 100,
      taxon_name = "Abies alba",
      pollen_counts = 2
    )

  for (
    sel_col in vec_required
  ) {
    data_missing <-
      dplyr::select(
        data_full,
        -dplyr::all_of(sel_col)
      )

    testthat::expect_error(
      harmonise_taxa(
        data_to_harmonize = data_missing,
        harmonisation_table = data_harm,
        level = "genus"
      ),
      "data must contain"
    )
  }
})

testthat::test_that("harmonise_taxa() rejects non-numeric pollen_counts", {
  data_input <-
    tibble::tibble(
      dataset_id = 1,
      age = 100,
      taxon_name = "Abies alba",
      pollen_counts = "2"
    )

  data_harm <-
    tibble::tibble(
      taxon_name = "Abies alba",
      genus = "Abies"
    )

  testthat::expect_error(
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    ),
    "'pollen_counts' in data_to_harmonize must be numeric"
  )
})

testthat::test_that("harmonise_taxa() validates harmonisation_table", {
  data_input <-
    tibble::tibble(
      dataset_id = 1,
      age = 100,
      taxon_name = "Abies alba",
      pollen_counts = 2
    )

  testthat::expect_error(
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = NULL,
      level = "genus"
    ),
    "harmonisation_table has to be data.frame"
  )

  testthat::expect_error(
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = c(taxon_name = "Abies alba"),
      level = "genus"
    ),
    "harmonisation_table has to be data.frame"
  )

  data_harm_no_taxon <-
    tibble::tibble(
      original = "Abies alba",
      genus = "Abies"
    )

  testthat::expect_error(
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm_no_taxon,
      level = "genus"
    ),
    "harmonisation_table must contain 'taxon_name'"
  )
})

testthat::test_that("harmonise_taxa() validates level", {
  data_input <-
    tibble::tibble(
      dataset_id = 1,
      age = 100,
      taxon_name = "Abies alba",
      pollen_counts = 2
    )

  data_harm <-
    tibble::tibble(
      taxon_name = "Abies alba",
      genus = "Abies",
      family = "Pinaceae"
    )

  list_bad_level <-
    base::list(
      NA_character_,
      1,
      c("genus", "family"),
      character(0),
      NULL
    )

  for (
    sel_level in list_bad_level
  ) {
    testthat::expect_error(
      harmonise_taxa(
        data_to_harmonize = data_input,
        harmonisation_table = data_harm,
        level = sel_level
      ),
      "level must be a single character string"
    )
  }

  testthat::expect_error(
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "species"
    ),
    "harmonisation_table must contain 'taxon_name' and the 'level' column"
  )
})


#----------------------------------------------------------#
# Output structure -----
#----------------------------------------------------------#

testthat::test_that("harmonise_taxa() returns a tibble with fixed columns", {
  data_input <-
    tibble::tibble(
      dataset_id = c(1, 1, 2),
      age = c(100, 100, 200),
      taxon_name = c("Abies alba", "Alnus glutinosa", "Abies alba"),
      pollen_counts = c(2, 3, 4),
      extra_col = c("a", "b", "c")
    )

  data_harm <-
    tibble::tibble(
      taxon_name = c("Abies alba", "Alnus glutinosa"),
      genus = c("Abies", "Alnus"),
      family = c("Pinaceae", "Betulaceae")
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("dataset_id", "age", "taxon_name", "pollen_counts")
  )
  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_false(dplyr::is_grouped_df(res))
})


#----------------------------------------------------------#
# Functional correctness -----
#----------------------------------------------------------#

testthat::test_that("harmonise_taxa() sums counts of merged taxa", {
  data_input <-
    tibble::tibble(
      dataset_id = c(1, 1, 1, 1),
      age = c(100, 100, 100, 200),
      taxon_name = c(
        "Abies alba", "Abies grandis", "Alnus glutinosa", "Abies alba"
      ),
      pollen_counts = c(2, 5, 7, 11)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = c("Abies alba", "Abies grandis", "Alnus glutinosa"),
      genus = c("Abies", "Abies", "Alnus")
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    ) |>
    dplyr::arrange(age, taxon_name)

  data_expected <-
    tibble::tibble(
      dataset_id = c(1, 1, 1),
      age = c(100, 100, 200),
      taxon_name = c("Abies", "Alnus", "Abies"),
      pollen_counts = c(2 + 5, 7, 11)
    )

  testthat::expect_equal(res, data_expected)
})

testthat::test_that("harmonise_taxa() preserves the total pollen count", {
  data_input <-
    tibble::tibble(
      dataset_id = c(1, 1, 2, 2, 2),
      age = c(100, 100, 100, 300, 300),
      taxon_name = c("A1", "A2", "B1", "A1", "B1"),
      pollen_counts = c(1, 2, 3, 4, 5)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = c("A1", "A2", "B1"),
      genus = c("A", "A", "B")
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    )

  testthat::expect_equal(
    base::sum(dplyr::pull(res, pollen_counts)),
    base::sum(dplyr::pull(data_input, pollen_counts))
  )
})

testthat::test_that("harmonise_taxa() uses the column selected by level", {
  data_input <-
    tibble::tibble(
      dataset_id = c(1, 1),
      age = c(100, 100),
      taxon_name = c("Abies alba", "Picea abies"),
      pollen_counts = c(2, 3)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = c("Abies alba", "Picea abies"),
      genus = c("Abies", "Picea"),
      family = c("Pinaceae", "Pinaceae")
    )

  res_genus <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    )

  res_family <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "family"
    )

  testthat::expect_equal(
    base::sort(dplyr::pull(res_genus, taxon_name)),
    c("Abies", "Picea")
  )
  testthat::expect_equal(dplyr::pull(res_family, taxon_name), "Pinaceae")
  testthat::expect_equal(dplyr::pull(res_family, pollen_counts), 5)
})

testthat::test_that("harmonise_taxa() keeps datasets and ages separate", {
  data_input <-
    tibble::tibble(
      dataset_id = c(1, 2, 1),
      age = c(100, 100, 200),
      taxon_name = c("A1", "A1", "A1"),
      pollen_counts = c(1, 10, 100)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = "A1",
      genus = "A"
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    ) |>
    dplyr::arrange(dataset_id, age)

  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_equal(dplyr::pull(res, pollen_counts), c(1, 100, 10))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("harmonise_taxa() errors when a taxon is not in table", {
  data_input <-
    tibble::tibble(
      dataset_id = c(1, 1),
      age = c(100, 100),
      taxon_name = c("Abies alba", "Alnus glutinosa"),
      pollen_counts = c(2, 3)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = "Abies alba",
      genus = "Abies"
    )

  testthat::expect_error(
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    ),
    "harmonisation table must contain taxon_names"
  )
})

testthat::test_that("harmonise_taxa() errors on ambiguous table mapping", {
  data_input <-
    tibble::tibble(
      dataset_id = 1,
      age = 100,
      taxon_name = "Abies alba",
      pollen_counts = 2
    )

  data_harm <-
    tibble::tibble(
      taxon_name = c("Abies alba", "Abies alba"),
      genus = c("Abies", "Picea")
    )

  testthat::expect_error(
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    ),
    "there is duplication of taxon_name in harm table"
  )
})

testthat::test_that("harmonise_taxa() ignores exact duplicate table rows", {
  data_input <-
    tibble::tibble(
      dataset_id = c(1, 1),
      age = c(100, 100),
      taxon_name = c("Abies alba", "Alnus glutinosa"),
      pollen_counts = c(5, 7)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = c("Abies alba", "Abies alba", "Alnus glutinosa"),
      genus = c("Abies", "Abies", "Alnus")
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    ) |>
    dplyr::arrange(taxon_name)

  testthat::expect_equal(dplyr::pull(res, pollen_counts), c(5, 7))
})

testthat::test_that("harmonise_taxa() allows extra taxa in the table", {
  data_input <-
    tibble::tibble(
      dataset_id = 1,
      age = 100,
      taxon_name = "Abies alba",
      pollen_counts = 2
    )

  data_harm <-
    tibble::tibble(
      taxon_name = c("Abies alba", "Quercus robur"),
      genus = c("Abies", "Quercus")
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    )

  testthat::expect_equal(dplyr::pull(res, taxon_name), "Abies")
})

testthat::test_that("harmonise_taxa() handles a single-row input", {
  data_input <-
    tibble::tibble(
      dataset_id = 1,
      age = 10,
      taxon_name = "Abies alba",
      pollen_counts = 5
    )

  data_harm <-
    tibble::tibble(
      taxon_name = "Abies alba",
      genus = "Abies"
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, taxon_name), "Abies")
  testthat::expect_equal(dplyr::pull(res, pollen_counts), 5)
})

testthat::test_that("harmonise_taxa() returns zero rows for empty data", {
  data_input <-
    tibble::tibble(
      dataset_id = numeric(0),
      age = numeric(0),
      taxon_name = character(0),
      pollen_counts = numeric(0)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = "Abies alba",
      genus = "Abies"
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(
    res,
    c("dataset_id", "age", "taxon_name", "pollen_counts")
  )
})

testthat::test_that("harmonise_taxa() propagates NA counts in sums", {
  data_input <-
    tibble::tibble(
      dataset_id = c(1, 1),
      age = c(100, 100),
      taxon_name = c("A1", "A2"),
      pollen_counts = c(1, NA)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = c("A1", "A2"),
      genus = c("A", "A")
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    )

  testthat::expect_true(base::is.na(dplyr::pull(res, pollen_counts)))
})

testthat::test_that("harmonise_taxa() groups NA harmonised names together", {
  data_input <-
    tibble::tibble(
      dataset_id = c(1, 1, 1),
      age = c(100, 100, 100),
      taxon_name = c("A1", "X1", "X2"),
      pollen_counts = c(1, 2, 3)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = c("A1", "X1", "X2"),
      genus = c("A", NA, NA)
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    )

  data_na <-
    dplyr::filter(res, base::is.na(taxon_name))

  testthat::expect_equal(base::nrow(res), 2L)
  testthat::expect_equal(dplyr::pull(data_na, pollen_counts), 5)
})


#----------------------------------------------------------#
# Scale -----
#----------------------------------------------------------#

testthat::test_that("harmonise_taxa() handles a moderately large input", {
  base::set.seed(900723)

  vec_taxa <-
    base::sprintf("taxon_%02d", base::seq_len(20))

  data_input <-
    tidyr::expand_grid(
      dataset_id = base::seq_len(10),
      age = base::seq(from = 0, to = 2400, by = 100),
      taxon_name = vec_taxa
    ) |>
    dplyr::mutate(
      pollen_counts = stats::rpois(n = dplyr::n(), lambda = 10)
    )

  data_harm <-
    tibble::tibble(
      taxon_name = vec_taxa,
      genus = base::rep(c("A", "B", "C", "D"), times = 5)
    )

  res <-
    harmonise_taxa(
      data_to_harmonize = data_input,
      harmonisation_table = data_harm,
      level = "genus"
    )

  testthat::expect_equal(base::nrow(data_input), 5000L)
  testthat::expect_equal(base::nrow(res), 10L * 25L * 4L)
  testthat::expect_equal(
    base::sum(dplyr::pull(res, pollen_counts)),
    base::sum(dplyr::pull(data_input, pollen_counts))
  )
})
