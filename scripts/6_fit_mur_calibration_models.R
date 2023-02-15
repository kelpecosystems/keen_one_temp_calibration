#' ---------------------------------------------------
#' Make MUR and Logger Fits
#' ---------------------------------------------------

library(readr)
library(dplyr)
library(purrr)
library(ggplot2); theme_set(theme_minimal(base_size = 12)) #for viz
library(patchwork)
library(performance)
library(broom)

joined_dat <- read_csv("derived_data/joined_calibration_mur_data.csv")

get_r2 <- function(mod) mod |> r2() |> unlist() %>% `[`(1)

# get model fits and r2
dat_with_fits <- joined_dat |>
  mutate(month = as.character(month)) |>
  group_by(site) |>
  nest() |>
  summarize(linear_mods = map(data, ~lm(temp_c_logger ~ temp_c_mur*month,
                                        data = .,)),
            loglog_mods = map(data, ~lm(log(temp_c_logger) ~ 
                                              log(temp_c_mur)*month,
                                        data = .,)),
            linear_mods_nomo = map(data, ~lm(temp_c_logger ~ temp_c_mur,
                                        data = .,)),
            loglog_mods_nomo = map(data, ~lm(log(temp_c_logger) ~ 
                                          log(temp_c_mur),
                                        data = .,))) |>
  mutate(linear_r2 = map_dbl(linear_mods, get_r2),
         loglog_r2 = map_dbl(loglog_mods, get_r2),
         linear_r2_nomo = map_dbl(linear_mods_nomo, get_r2),
         loglog_r2_nomo = map_dbl(loglog_mods_nomo, get_r2),
  )

# write our model fits
saveRDS(dat_with_fits, "derived_data/mur_calibration_models.rds")

# look for bias in fits
dat_pred_obs <- dat_with_fits |>
  group_by(site) |>
  reframe(map_df(loglog_mods, augment)) 


# plot fitted against actual values to check bias
ggplot(dat_pred_obs,
       aes(x = exp(`log(temp_c_logger)`), y = exp(.fitted), 
           color = site)) +
  facet_wrap(vars(site)) +
  geom_point() +
  labs(color = "", x = "Logger Temperature (C)",
       y = "Predicted Temperature from MUR SST 4.1") +
  ggthemes::scale_color_calc(guide = "none") +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  geom_label(data = dat_with_fits |> mutate(x=5, y = 20),
             aes(x=x, y=y, 
                 label = paste0("r\u00B2=",round(loglog_r2,2))),
                 color = "black",label.size=0)
  
ggsave("figures/fit.jpg")
# plot residuals against actual values to check bias

ggplot(dat_pred_obs,
       aes(x = `log(temp_c_logger)`, y = .resid, 
           color = site)) +
  facet_wrap(vars(site), scale = "free_x") +
  geom_point() +
  labs(color = "", x = "Log Logger Temperature (C)",
       y = "Model Residual") +
  ggthemes::scale_color_calc(guide = "none") 
  
ggsave("figures/residuals.jpg")


