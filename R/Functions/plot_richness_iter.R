# Plot richness for single iteration

plot_richness_iter <- function(data_back,data_rich,iter) {
  
  data_back[[iter]] %>% 
    readr::read_rds() %>% 
    ggplot(aes(x = age, y = richness)) +
    geom_line(data = readr::read_rds(data_rich[[iter]]), aes(group = dataset_id), linewidth = 0.1, alpha = 0.1) +
    geom_line(linewidth = 1, color= "red") +
    geom_ribbon(aes(ymin = rich_low, ymax = rich_high, y = NULL),       fill = "blue", alpha = 0.4) +
    facet_wrap(~ region) +
    theme_classic() +
    scale_x_reverse()
}