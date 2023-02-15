#' ---------------------------------------------------
#' Visualize MUR and Logger Fits
#' ---------------------------------------------------

library(readr)
library(ggplot2); theme_set(theme_minimal(base_size = 12)) #for viz
library(patchwork)
library(dplyr)

joined_dat <- read_csv("derived_data/joined_calibration_mur_data.csv")



linear_nofit <- ggplot(joined_dat,
                  aes(x = temp_c_mur, y = temp_c_logger,
                      color = site)) +
  facet_wrap(vars(site)) +
  geom_point() +
  labs(color = "", y = "Logger Temperature (C)",
       x = "MUR 4.1 SST Temperature (C)",
       subtitle = "Raw Data with 1:1 line for reference") +
  ggthemes::scale_color_calc(guide = "none") +
  geom_abline(slope = 1, intercept = 0, lty = 2)

log_fit <- ggplot(joined_dat,
       aes(x = temp_c_mur, y = temp_c_logger,
           color = site)) +
  facet_wrap(vars(site)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(color = "", y = "Logger Temperature (C)",
       x = "MUR 4.1 SST Temperature (C)",
       subtitle = "Log-Log Plot with linear model fits") +
  ggthemes::scale_color_calc(guide = "none") +
  stat_smooth(color = "black", method = "lm")


combined_plot <- linear_nofit + log_fit + plot_layout(guides = "collect")
ggsave("figures/calibration_plot.jpg", combined_plot, width = 12)


#### Show timeseries ####
joined_dat |>
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) |>
  tidyr::pivot_longer(c(temp_c_logger, temp_c_mur),
                      values_to = "temp_c", names_to = "source") |>
  ggplot(aes(x = datetime, y = temp_c, color = source)) +
  geom_point(alpha = 0.5) +
  facet_wrap(vars(site), scale = "free_x") +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("Benthic Loggers", "MUR 4.1 SST")) +
  labs(x = "", y = "Temperature (C)", color = "Data Source")
ggsave("figures/mur_calibration_timeseries.jpg")
