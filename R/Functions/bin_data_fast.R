bin_data_fast <- function(data_source, binning_var, bin_size) {
  
  #------------------------------------------------------#
  # 0. Validation (kept minimal for speed)
  #------------------------------------------------------#
  
  assertthat::assert_that(is.data.frame(data_source))
  
  required_cols <- c("age", "pollen_counts", "taxa")
  missing_cols <- setdiff(required_cols, colnames(data_source))
  assertthat::assert_that(length(missing_cols) == 0)
  
  bin_col <- rlang::ensym(binning_var)
  bin_name <- rlang::as_string(bin_col)
  
  assertthat::assert_that(bin_name %in% colnames(data_source))
  assertthat::assert_that(is.numeric(bin_size), length(bin_size) == 1, bin_size > 0)
  
  #------------------------------------------------------#
  # 1. FAST binning (no cut())
  #------------------------------------------------------#
  
  age <- data_source$age
  min_age <- min(age)
  
  # integer bin index (FAST)
  bin_id <- ((age - min_age) %/% bin_size) + 1L
  
  #------------------------------------------------------#
  # 2. Fast aggregation
  #------------------------------------------------------#
  
  data_source$BIN <- bin_id
  
  data_binned <- data_source %>% 
    dplyr::group_by(.data[[bin_name]], taxa, BIN) %>% 
    dplyr::summarise(
      summed_pollen_count = sum(pollen_counts),
      .groups = "drop"
    )
  
  return(data_binned)
}