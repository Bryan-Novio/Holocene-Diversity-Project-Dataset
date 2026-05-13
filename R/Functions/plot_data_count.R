#@Count the no. of data points of initial data in each step

plot_data_count <- 
  function(data) {
    data %>% 
      summarise(N = n()) %>% 
      mutate(data = as.character(1)) %>% 
      ggplot(aes(x = data, y = N)) + 
      geom_col()
  }

