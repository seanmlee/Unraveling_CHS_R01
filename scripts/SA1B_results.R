
# SA1A expected results --------------------------------------------------------


# load library -----------------------------------------------------------------
library(tidyverse)


# sample data ------------------------------------------------------------------
time_points <- seq(0, 10, length.out = 5)

abstinence <- time_points^2 * 0.02
heavy_use <- -time_points^2 * 0.02

data <- data.frame(
  Time = rep(time_points, 2),
  HRQOL = c(abstinence, heavy_use),
  Group = rep(c("Abstinence", "Continued Heavy Use"), each = length(time_points))
)

data2 <- data.frame(
  Time = rep(time_points, 2),
  HRQOL = c(abstinence, heavy_use) + 5,
  Group = rep(c("Abstinence", "Continued Heavy Use"), each = length(time_points))
)

data$type <- "CHS"
data2$type <- "Non-CHS"

data <- rbind(data, data2)


# plot -------------------------------------------------------------------------
ggplot(data, aes(x = Time, y = HRQOL, linetype = Group, color = Group, fill = Group)) +
  
  geom_line(size = 1.25) +
  
  geom_point(size = 4, alpha = 0.75, pch = 21, color = "black") +
  
  scale_y_continuous(name = "HRQOL", breaks = NULL) +
  
  scale_x_continuous(name = "Time", breaks = NULL) +
  theme_minimal() +
  
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    legend.title = element_blank()
  ) +
  
  labs(
    title = "1B. Expected Results",
    linetype = ""
  ) +
  
  theme_bw() +
  
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 18, hjust = 0.5),
    strip.text = element_text(size = 18),
    strip.background = element_rect(fill = NA)
  ) +
  
  scale_y_continuous(name = "Quality of Life", breaks = NULL) +
  
  guides(
    linetype = guide_legend(nrow=2, byrow=TRUE),
    color = "none",
    fill = "none"
  ) +
  
  facet_grid(~type)
