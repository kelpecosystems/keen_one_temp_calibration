#' ---------------------------------------------------
#' Visualize MUR and Logger Fits
#' ---------------------------------------------------

library(readr)
library(ggplot2)
library(patchwork)

joined_dat <- read_csv("derived_data/joined_calibration_mur_data.csv")



linear_nofit <- ggplot(joined_dat,
                  aes(x = temp_c_mur, y = temp_c,
                      color = site)) +
  facet_wrap(vars(site)) +
  geom_point() +
  labs(color = "", y = "Logger Temperature (C)",
       x = "MUR 4.1 SST Temperature (C)",
       subtitle = "Raw Data with 1:1 line for reference") +
  ggthemes::scale_color_tableau(guide = "none") +
  geom_abline(slope = 1, intercept = 0, lty = 2)

log_fit <- ggplot(joined_dat,
       aes(x = temp_c_mur, y = temp_c,
           color = site)) +
  facet_wrap(vars(site)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(color = "", y = "Logger Temperature (C)",
       x = "MUR 4.1 SST Temperature (C)",
       subtitle = "Log-Log Plot with linear model fits") +
  ggthemes::scale_color_tableau(guide = "none") +
  stat_smooth(color = "black", method = "lm")


combined_plot <- linear_nofit + log_fit + plot_layout(guides = "collect")
ggsave("figures/calibration_plot.jpg", combined_plot, width = 12)
