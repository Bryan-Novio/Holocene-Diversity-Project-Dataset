plot_trend_general <- function(data, color) {
  data %>% 
  ggplot(aes(x = age, y = est)) +
  geom_line(linewidth = 3, color = color) + 
  geom_ribbon(aes(ymin = low, 
                  ymax = upp),  fill = color, alpha = 0.1) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 25, color = "black",angle = 90, vjust = 0.7),
        axis.text.y.right = element_text(size = 25, color = "black",angle = 90, vjust = 0.7, hjust = 0.5),
        axis.line.y.right = element_line(color = "black", linewidth = 1),
        axis.line.x.bottom = element_line(color = "black", linewidth = 1),
        axis.ticks.x = element_line(linewidth = 1),
        axis.ticks.y = element_line(linewidth = 1),
        axis.ticks.length.y.right = unit(.25, "cm"),
        axis.ticks.length.x.bottom = unit(.25, "cm"),
        panel.background = element_blank(),
  )  +
  scale_x_continuous(breaks = c(12000,8000, 4000, 0)) +
  scale_y_continuous(position = "right") 
}