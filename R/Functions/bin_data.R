bin_data <- function(data_source, binning_var, bin_size){
  
  #------------------------------------------------------#
  # 0. Minimal input validation additions (required)
  #------------------------------------------------------#
  
  # Check data_source is a data.frame (your original)
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_source has to be a data.frame"
  )
  
  # NEW: check required columns exist
  required_cols <- c("age", "pollen_counts", "taxa")
  missing_cols <- setdiff(required_cols, colnames(data_source))
  assertthat::assert_that(
    length(missing_cols) == 0,
    msg = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
  )
  
  # NEW: tidy-eval conversion so binning_var works as bare or string
  bin_col <- rlang::ensym(binning_var)
  assertthat::assert_that(
    rlang::as_string(bin_col) %in% colnames(data_source),
    msg = "binning_var column is missing in data_source"
  )
  
  # Check bin_size (your original)
  assertthat::assert_that(
    is.numeric(bin_size) && length(bin_size) == 1 && bin_size > 0,
    msg = "bin_size must be a single positive numeric value"
  )
  
  # NEW: reject NA in critical columns (Test 9)
  assertthat::assert_that(
    !any(is.na(data_source$age)) && !any(is.na(data_source$pollen_counts)),
    msg = "age and pollen_counts cannot contain NA values"
  )
  
  data_binned <- data_source %>% 
    mutate(
      BIN = cut(age, seq(min(age), max(age) + bin_size, bin_size), right = FALSE),
      
      BIN_chr = as.character(BIN),
      BIN_fct = as.factor(BIN_chr),
      BIN_int = as.factor(as.numeric(BIN_fct)), # recode bins to integer, then factor
      BIN = BIN_int
    ) %>% 
    group_by({{binning_var}}, taxa, BIN, BIN_chr) %>% 
    summarise(summed_pollen_count = sum(pollen_counts), .groups = "drop")
  
  return(data_binned)
}


