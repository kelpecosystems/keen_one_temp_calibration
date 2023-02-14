#' ---------------------------------------------------
#' Merge  calibration data and create a timeseries
#' 
#' 
#' TODO: some timezone issues with loggers perhaps - check
#' ---------------------------------------------------


library(readxl)
library(readr)
library(dplyr)
library(glue)
library(lubridate)
library(ggplot2); theme_set(theme_minimal(base_size = 12)) #for viz

# functions

f_to_c <- function(temp_f) (temp_f-32)*5/9
c_to_f <- function(temp_c) (temp_c*9/3-32)

read_hobo_f <- function(afile, dropcol = TRUE){
  adf <- read_csv(afile) 
  if(dropcol){adf <- adf |> select(-1)}
  names(adf) <- c("datetime", "temp_f")
  
  adf <- adf |>
    mutate(datetime = mdy_hm(datetime),
           temp_f = as.numeric(temp_f),
           temp_c = (temp_f-32)*5/9)
    
   adf |>
     filter(!is.na(datetime))
}

#### Load the calibration data ####
dijkstra <- bind_rows(read_hobo_f("raw_data/dijkstra/40mPlot_ALL.csv"),
                      read_hobo_f("raw_data/dijkstra/60mPlot_ALL.csv")) |>
  mutate(site = "Nubble Lighthouse")

# RI

humphries <- read_hobo_f("raw_data/humphries/Ft_Weatherill_16-20 temp.csv",
                         dropcol = FALSE) |>
  mutate(site = "Fort Weatherhill")

# hurricane island
h1 <- read_excel("raw_data/hurricane/Cage_5_9.22.19_6.3.20.xlsx",
                 skip = 1) |>
  select(2:3) |>
  rename_with(~ c("datetime", "temp_f")) |>
  filter(!(temp_f>62)) #start/end

h2 <- read_excel("raw_data/hurricane/Cage_5_2017_2019.xlsx")|>
  select(1:2) |>
  rename_with(~ c("datetime", "temp_f"))|>
  filter(!(temp_f>62)) #start/end

h3 <- read_excel("raw_data/hurricane/HI_NetBottom_06.22.2020_09.01.2020.xlsx",
                 skip = 1)|>
  select(2:3) |>
  rename_with(~ c("datetime", "temp_f"))|>
  filter(!(temp_f>62)) #start/end

h4 <- read_excel("raw_data/hurricane/UMaineSKNet_6.28.2019_9.8.2019.xlsx",
                 skip = 1)|>
  select(2:3) |>
  rename_with(~ c("datetime", "temp_f"))|>
  filter(!(temp_f>62)) #start/end

hurricane <- bind_rows(h1, h2, h3, h4) |>
  mutate(temp_c = f_to_c(temp_f),
         site = "Hurricane Island")



# Pemaquid
pemaquid <- read_excel("raw_data/MacMahan/Pemaquid_KEEN_2.xlsx",
                       skip = 1)|>
  select(2:3) |>
  rename_with(~ c("datetime", "temp_f"))|>
  mutate(site = "Pemaquid",
         temp_c = f_to_c(temp_f)) |>
  filter(!(temp_f>70),
         datetime < mdy("07-17-2019"),
         datetime > mdy("07-14-2018"),
  ) #start/end
  
# Schoodic
schoodic <- read_excel("raw_data/rasher/HoboMaine.xls")|>
  select(1:2) |>
  rename_with(~ c("datetime", "temp_c"))|>
  mutate(datetime = mdy_hms(gsub(",0$", "", datetime))) |>
  mutate(site = "Schoodic",
         temp_f = c_to_f(temp_c)) |>
  filter(datetime > mdy("09-12-2002"))

# Byrnes lab sites
byrnes <- read_csv("raw_data/byrnes/processed_data/merged_hobo_data_with_sites.csv") |>
  rename(site = Site) |>
  select(datetime, site, temp_c)

# check
# ggplot(schoodic,
#        aes(x = datetime, y = temp_f, color=site)) +
#   geom_point()

#### Bind it together and write out ####
calibration <- bind_rows(byrnes,
                         schoodic,
                         pemaquid,
                         hurricane,
                         dijkstra,
                         humphries) |>
  filter(!is.na(site)) |>
  group_by(day = day(datetime), month = month(datetime), 
           year = year(datetime), site) |>
  summarize(temp_c = mean(temp_c), .groups = "drop") |>
  mutate(datetime = as.Date(glue("{year}-{month}-{day}")))

 ggplot(calibration,
        aes(x = datetime, y = temp_c, color=site)) +
   geom_point() 

write_csv(calibration, "derived_data/calibration_data_only.csv")
