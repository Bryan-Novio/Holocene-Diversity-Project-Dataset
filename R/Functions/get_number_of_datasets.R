#function to get number of datasets

get_number_of_datasets <- function(data_source, group_var = NULL, name = NULL) {
  
  assertthat::assert_that(
    is.data.frame(data_source)
  )
  
  assertthat::assert_that(
    "dataset_id" %in%  names(data_source) 
  )
  
  
  if (
    !is.null(group_var)
  ) {
    assertthat::assert_that(
      group_var %in%  names(data_source) 
    )
    
    data_source <- 
      data_source %>% 
      dplyr::group_by(get(group_var))
  }
  
  
  data_count <- 
    data_source %>% 
    distinct(dataset_id) %>% 
    dplyr::count() 
  
  if(
    isFALSE(is.null(name))
  ) {
    
    assertthat::assert_that(
      is.character(name)
    )
    
    res <- 
      data_count %>% 
      mutate(data = as.character(name))
      
    
  } else {
    res <- 
      data_count 
  }
  
  
  return(res)
}

