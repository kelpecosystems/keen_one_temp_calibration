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
source("scripts/load_sst_mur.R")


#### Join the MUR and calibration data ####

# checking we can join
sc <- unique(calibration$site) |> sort()
sm <- unique(sst_mur$site)|> sort()

scb <- sc[!(sc %in% sm)]
smb <- sm[!(sm %in% sc)]

print("Sites not matching...")
scb

print("Anti-Join results of calibration v. sst_mur...")
anti_join(calibration, sst_mur)


# do the join
joined_dat <- left_join(calibration, sst_mur) |>
  filter(!is.nan(temp_c_mur)) # some weird 0 values in MUR product

js <- unique(joined_dat$site) |> sort()

##### Write the joined data ####
write_csv(joined_dat, "derived_data/joined_calibration_mur_data.csv")



