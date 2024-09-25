
# SA1A expected results --------------------------------------------------------


# load library -----------------------------------------------------------------
library(tidyverse)
library(ggsignif)


# sample data ------------------------------------------------------------------
data <- data.frame(
  HeavyUse = factor(c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No")),
  CHS = c(1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1)
)


# fit logistic model -----------------------------------------------------------
model <- glm(
  CHS ~ 
    HeavyUse
  , 
  data = data, 
  family = binomial
  )


# generate predicted data ------------------------------------------------------
new_data <- data.frame(
  HeavyUse = factor(c("Yes", "No"), levels = levels(data$HeavyUse))
  )

predictions <- predict(model, newdata = new_data, type = "response")

plot_data <- data.frame(
  HeavyUse = levels(data$HeavyUse),
  Probability = predictions
)


# plot -------------------------------------------------------------------------
ggplot(
  
  plot_data, 
  
  aes(
    x = HeavyUse, 
    y = Probability,
    fill = HeavyUse
    )
  ) +
  
  geom_errorbar(
    aes(
      ymin = Probability - 0.1, 
      ymax = Probability + 0.1), 
    width = 0.05,
    size = 0.5
    ) +
  
  geom_point(
    size = 8,
    pch = 21,
    color = "black",
  ) +
  
  labs(
    title = "1A. Expected Results",
    x = "Heavy Use", 
    y = "Probability of CHS"
    ) +
  
  scale_y_continuous(
    labels = c("0" = "Low", "1" = "High"),
    breaks = c(0, 1),
    limits = c(0, 1)
  ) +
  
  theme_bw() +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 18), 
    axis.text = element_text(size = 18),
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.position = "none"
  ) +
  
  # reverse, i mistakenly reversed above
  scale_x_discrete(
    labels = c("No" = "Yes", "Yes" = "No")
  )
