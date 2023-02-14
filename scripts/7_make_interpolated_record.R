#' ---------------------------------------------------
#' Make Full Interpolated Timeseries
#' ---------------------------------------------------

library(readr)
library(dplyr)
library(purrr)
library(ggplot2); theme_set(theme_minimal(base_size = 12)) #for viz


source("scripts/load_sst_mur.R")
sst_mur <- sst_mur |>
  filter(year >= 2014)

joined_dat <- read_csv("derived_data/joined_calibration_mur_data.csv")

dat_with_fits <- readRDS("derived_data/mur_calibration_models.rds")

#### Create interpolated temperatures to fill gaps ####

#### Visualize combined timeseries records by site ####
