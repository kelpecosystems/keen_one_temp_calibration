#' ---------------------------------------------------
#' Make Full Interpolated Timeseries
#' ---------------------------------------------------

library(readr)
library(dplyr)
library(purrr)
library(glue)
library(tidyr)
library(ggplot2); theme_set(theme_minimal(base_size = 12)) #for viz


source("scripts/load_sst_mur.R")
sst_mur <- sst_mur |>
  filter(year >= 2014)

calibration_dat <- read_csv("derived_data/joined_calibration_mur_data.csv") |>
  select(day, month, year, site, temp_c_logger)

dat_with_fits <- readRDS("derived_data/mur_calibration_models.rds") |>
  select(site, loglog_mods)

sst_joined <- left_join(sst_mur, calibration_dat) |>
  mutate(month = as.character(month)) #for prediction

#### Create interpolated temperatures to fill gaps ####
sst_joined <- sst_joined |>
  group_by(site) |>
  nest() |>
  left_join(dat_with_fits) |>
  filter(!site %in% c("Pemaquid", "Nahant", "Little Brewster")) |> #problem sites
  #calculate and expand predicted values along with data
  mutate(predicted = map2(loglog_mods, data, ~
                            data.frame(predicted = 
                                         exp(predict(.x, newdata = .y))))) |>
  unnest(c(data, predicted))


#check
# ggplot(sst_joined,
#        aes(x = temp_c_logger, 
#            y = predicted, color = site)) +
#   geom_point()

# create the combined record
sst_interpolated <- sst_joined |>
  select(site, year, month, day, temp_c_logger, predicted) |>
  mutate(temp_c = temp_c_logger,
         source = ifelse(is.na(temp_c), "modeled", "observed"),
         temp_c = ifelse(is.na(temp_c), predicted, temp_c),
         datetime = as.Date(paste(year, month, day, sep = "-"))
  )


#### Visualize combined timeseries records by site ####

ggplot(sst_interpolated,
       aes(x = datetime,
           y = temp_c, color = source)) +
  geom_point() +
  facet_wrap(vars(site)) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("observed", "modeled")) +
  labs(x = "", y = "Temperature (C)", color = "Data Source")
ggsave("figures/modeled_timeseries.jpg", width = 8)
