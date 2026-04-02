bin_data_dt <- function(data_source, binning_var, bin_size) {
  
  dt <- data.table::as.data.table(data_source)
  
  min_age <- min(dt$age)
  dt[, BIN := ((age - min_age) %/% bin_size) + 1L]
  
  bin_name <- rlang::as_string(rlang::ensym(binning_var))
  
  as_tibble(dt[
    , .(summed_pollen_count = sum(pollen_counts)),
    by = c(bin_name, "taxa", "BIN")
  ])
  
}
