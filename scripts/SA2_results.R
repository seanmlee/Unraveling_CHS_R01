
# SA2 expected results ---------------------------------------------------------


# load library -----------------------------------------------------------------
library(tidyverse)


# sample data ------------------------------------------------------------------
data <- data.frame(
  Category = rep(c("Acute Episode", "Between Episode"), each = 2),
  CHS = rep(c("Yes", "No"), 2),
  Activity = c(1, 0.25, 0.5, 0.25)
)

data$CHS <- factor(data$CHS, levels = c("Yes", "No"))


# plot -------------------------------------------------------------------------
data %>%
  
  ggplot(
    aes(x = Category, y = Activity, color = CHS, fill = CHS, linetype = CHS)
  ) +
  
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), alpha = 0.1, width = 0.5, size = 1) +
  
  labs(
    title = "SA2. Expected Results",
    x = "", 
    y = "Receptor Activity"
    ) +
  
  scale_y_continuous(
    labels = c("0" = "Low", "1" = "High"),
    breaks = c(0, 1),
    limits = c(0, 1)
  ) +
  
  scale_x_discrete(
    labels = c("Acute\nEpisode", "Between\nEpisode")
  ) +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 18), 
    axis.title = element_text(size = 18), 
    plot.title = element_text(size = 18),
    legend.position = "right",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)
  )
