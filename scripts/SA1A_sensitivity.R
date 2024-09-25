
# SA1A sensitivity analysis expected results -----------------------------------


# load library -----------------------------------------------------------------
library(ggplot2)
library(splines)


# assign delta and the corresponding odds ratios for the original curve --------
delta <- seq(0, 10, 1)


# original curve with peak at delta = 5
odds_ratio_original <- c(1, 1.1, 1.3, 1.6, 1.9, 2.0, 1.9, 1.6, 1.3, 1.1, 1)  # More moderate peak
widths <- c(0.3, 0.25, 0.2, 0.15, 0.1, 0.1, 0.15, 0.2, 0.25, 0.3)  # Symmetric width adjustment
ci_low_original <- odds_ratio_original - widths  # Lower bound with symmetric width
ci_high_original <- odds_ratio_original + widths  # Upper bound with symmetric width


# assign new curves with peaks at delta = 7 and delta = 3
odds_ratio_shifted_7 <- c(1, 1.0, 1.1, 1.3, 1.6, 1.9, 2.0, 1.9, 1.6, 1.3, 1.1)
odds_ratio_shifted_3 <- c(1.1, 1.3, 1.6, 1.9, 2.0, 1.9, 1.6, 1.3, 1.1, 1.0, 1)
ci_low_shifted_7 <- odds_ratio_shifted_7 - widths
ci_high_shifted_7 <- odds_ratio_shifted_7 + widths
ci_low_shifted_3 <- odds_ratio_shifted_3 - widths
ci_high_shifted_3 <- odds_ratio_shifted_3 + widths

sensitivity_data_original <- data.frame(
  delta = delta,
  odds_ratio = odds_ratio_original,
  ci_low = ci_low_original,
  ci_high = ci_high_original
)

sensitivity_data_shifted_7 <- data.frame(
  delta = delta,
  odds_ratio = odds_ratio_shifted_7,
  ci_low = ci_low_shifted_7,
  ci_high = ci_high_shifted_7
)

sensitivity_data_shifted_3 <- data.frame(
  delta = delta,
  odds_ratio = odds_ratio_shifted_3,
  ci_low = ci_low_shifted_3,
  ci_high = ci_high_shifted_3
)

fine_delta <- seq(min(delta), max(delta), length.out = 100)

spline_fit_original <- spline(
  x = sensitivity_data_original$delta,
  y = sensitivity_data_original$odds_ratio,
  xout = fine_delta
)
spline_ci_low_original <- spline(
  x = sensitivity_data_original$delta,
  y = sensitivity_data_original$ci_low,
  xout = fine_delta
)$y
spline_ci_high_original <- spline(
  x = sensitivity_data_original$delta,
  y = sensitivity_data_original$ci_high,
  xout = fine_delta
)$y

spline_fit_shifted_7 <- spline(
  x = sensitivity_data_shifted_7$delta,
  y = sensitivity_data_shifted_7$odds_ratio,
  xout = fine_delta
)
spline_ci_low_shifted_7 <- spline(
  x = sensitivity_data_shifted_7$delta,
  y = sensitivity_data_shifted_7$ci_low,
  xout = fine_delta
)$y
spline_ci_high_shifted_7 <- spline(
  x = sensitivity_data_shifted_7$delta,
  y = sensitivity_data_shifted_7$ci_high,
  xout = fine_delta
)$y

spline_fit_shifted_3 <- spline(
  x = sensitivity_data_shifted_3$delta,
  y = sensitivity_data_shifted_3$odds_ratio,
  xout = fine_delta
)
spline_ci_low_shifted_3 <- spline(
  x = sensitivity_data_shifted_3$delta,
  y = sensitivity_data_shifted_3$ci_low,
  xout = fine_delta
)$y
spline_ci_high_shifted_3 <- spline(
  x = sensitivity_data_shifted_3$delta,
  y = sensitivity_data_shifted_3$ci_high,
  xout = fine_delta
)$y

sensitivity_data_smooth_original <- data.frame(
  delta = fine_delta,
  odds_ratio = spline_fit_original$y,
  ci_low = spline_ci_low_original,
  ci_high = spline_ci_high_original
)

sensitivity_data_smooth_shifted_7 <- data.frame(
  delta = fine_delta,
  odds_ratio = spline_fit_shifted_7$y,
  ci_low = spline_ci_low_shifted_7,
  ci_high = spline_ci_high_shifted_7
)

sensitivity_data_smooth_shifted_3 <- data.frame(
  delta = fine_delta,
  odds_ratio = spline_fit_shifted_3$y,
  ci_low = spline_ci_low_shifted_3,
  ci_high = spline_ci_high_shifted_3
)


# plot -------------------------------------------------------------------------
ggplot() +
  
  geom_ribbon(data = sensitivity_data_smooth_original, aes(x = delta, ymin = ci_low, ymax = ci_high), fill = "black", alpha = 0.1) +
  
  geom_line(data = sensitivity_data_smooth_original, aes(x = delta, y = odds_ratio), color = "black", size = 0.5) +

  geom_ribbon(data = sensitivity_data_smooth_shifted_7, aes(x = delta, ymin = ci_low, ymax = ci_high), fill = "black", alpha = 0.1) +
  
  geom_line(data = sensitivity_data_smooth_shifted_7, aes(x = delta, y = odds_ratio), color = "black", size = 0.5) +
  
  geom_ribbon(data = sensitivity_data_smooth_shifted_3, aes(x = delta, ymin = ci_low, ymax = ci_high), fill = "black", alpha = 0.1) +
  
  geom_line(data = sensitivity_data_smooth_shifted_3, aes(x = delta, y = odds_ratio), color = "black", size = 0.5) +
  
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 1)
  ) +
  
  scale_y_continuous(
    limits = c(0, max(sensitivity_data_smooth_original$odds_ratio, sensitivity_data_smooth_shifted_7$odds_ratio, sensitivity_data_smooth_shifted_3$odds_ratio) + 1)
  ) +
  
  labs(
    title = "1A. Sensitivity Analysis",
    x = "Heavy Use Threshold",
    y = "Odds Ratio"
  ) +
  
  theme_bw() +
  
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),  
    plot.title = element_text(size = 18, hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank() 
  )
