plot_trend_study_2 <- function(data) {
  
  data %>% 
  ggplot2::ggplot(aes(x = age, y = est))+ 
    ggplot2::geom_line(color = "black", linewidth = 3
    ) +
    geom_ribbon(aes(ymin = low, ymax = upp), colour = "gray",alpha = 0.5
    ) +
    ggplot2::theme(axis.text.x = element_text(size = 25, color = "black",angle = 90, vjust = 0.7),
                   axis.text.y.right = element_text(size = 25, color = "black",angle = 90, vjust = 0.7, hjust = 0.5),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   panel.background = element_blank(),
                   axis.line.y.right  = element_line(color = "black", linewidth = 1),
                   axis.line.x.bottom = element_line(color = "black", linewidth = 1),
                   axis.ticks.x = element_line(linewidth = 1),
                   axis.ticks.y = element_line(linewidth = 1),
                   axis.ticks.length.y.right = unit(.25, "cm"),
                   axis.ticks.length.x.bottom = unit(.25, "cm"))  +
    ggplot2::coord_cartesian(
      ylim = c(6,14)
    ) +
    ggplot2::scale_x_continuous() +   
    scale_y_continuous(position = "right") +
    ggplot2::geom_vline(xintercept = 9500, color ="red", size = 2)
  
}

