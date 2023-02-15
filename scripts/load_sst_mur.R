require(glue)
require(lubridate)
require(dplyr)

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