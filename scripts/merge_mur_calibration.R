#' ---------------------------------------------------
#' Merge MUR site data with calibration data and create
#' a 2014 - 2022 Timeseries that combines valid and interpolated
#' temperatures
#' ---------------------------------------------------


library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(ggplot2); theme_set(theme_minimal(base_size = 12)) #for viz

#### Load the calibration data ####
calibration <- read_csv("derived_data/calibration_data_only.csv") |>
  rename(temp_c_logger = temp_c)

#### Load the MUR data ####
sst_mur <- map_df(c(2002, 2003, 2013:2022),
                  ~read_csv(glue("raw_data/MURSST41/{.x}.csv"))) |>
  mutate(day = day(Date),
         month = month(Date),
         year = year(Date)) |>
  rename(temp_c_mur = value,
         site = Site) |>
  select(site, temp_c_mur, Latitude, Longitude, month, day, year) |>
  mutate(site = gsub("Weatherill", "Weatherhill", site)) |>
  mutate(site = iconv(site, from = 'UTF-8', to = 'ASCII//TRANSLIT'))|> #some encoding issues
  # a few sites had their loggers moved
  group_by(site, month, day, year) |>
  summarize(Latitude = mean(Latitude),
            Longitude = mean(Longitude),
            temp_c_mur = mean(temp_c_mur, na.rm = TRUE),
            .groups = "drop")


#### Join the MUR and calibration data ####

# checking we can join
sc <- unique(calibration$site) |> sort()
sm <- unique(sst_mur$site)|> sort()

scb <- sc[!(sc %in% sm)]

print("Sites not matching...")
scb

print("Anti-Join results of calibration v. sst_mur...")
anti_join(calibration, sst_mur)


# do the join
joined_dat <- left_join(calibration, sst_mur) |>
  filter(temp_c_mur != 0) # some weird 0 values in MUR product

##### Write the joined data ####
write_csv(joined_dat, "derived_data/joined_calibration_mur_data.csv")


#### Fit models by site ####

#### Visualize fit models by site ####

#### Create interpolated temperatures to fill gaps ####

#### Visualize combined timeseries records by site ####
