#' ------------------------------------------------
#' Get 2014 - 2022 MUR SST data from ERDDAP for lat/longs
#' that match calibration data sites and save out the
#' resulting timeseries
#' 
#' ------------------------------------------------

library(sf)
library(readxl)
library(rnaturalearth)
library(ggplot2)
library(rerddap)
library(rerddapXtracto)
library(dplyr)
library(terra)
library(ncdf4)

#### Get a bounding box for New England and get Site Lat Longs ####
keen_bbox <- st_bbox(c(ymin = 41.0158484503146, 
                     xmin = -72.41734371426318,
                     ymax = 44.630811336788774, 
                     xmax = -65.96160946413212),
                     crs = 4326)

land <- ne_countries(scale = "large", returnclass = "sf") |>
  st_crop(keen_bbox)

ll_info <- read_excel("raw_data/lat_longs.xlsx") |>
  group_by(Latitude, Longitude) |>
  slice_head() |>
  ungroup() |>
  dplyr::select(Site, Latitude, Longitude) |>
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = 4326,
           remove = FALSE)


# check sites
ggplot() +
  geom_sf(data = land) +
  geom_sf(data = ll_info, color = "red") +
  theme_minimal(base_size = 14)


#### Get MUR Data ####
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.html
# jplMURSST41 is the dataset name

ERDDAP_Node <- "https://coastwatch.pfeg.noaa.gov/erddap/"


dataInfo <- rerddap::info('jplMURSST41', 
                          url=ERDDAP_Node)

# let's try it with extracting things from rasters
library(rerddap)

get_timeseries <- function(year,
                           pts_to_extract = ll_info,
                           product = 'jplMURSST41',
                           ERDDAP_Node = "https://coastwatch.pfeg.noaa.gov/erddap/",
                           variable_name = "analysed_sst",
                           latitude_bound = c(41.01585, 44.6308),
                           longitude_bound = c(-72.41734, -65.96161),
                           plot_check = FALSE,
                           out_path = "raw_data/MURSST41/",
                           sleep = 20){

  #create a date vector
  dates <- c(paste0(year, "-01-01"), paste0(year, "-12-31"))
  if(year==2002) dates[1] <- "2002-06-01"

  # get the data
  print(paste(year, product, variable_name, nrow(pts_to_extract), "points"))
  print("Getting data from ERDDAP")
  
  dat <- griddap(
    url=ERDDAP_Node,
    datasetx = product,
    time = dates,
    latitude = latitude_bound,
    longitude = longitude_bound,
    fields = variable_name
  )
  
  print("Loading data as raster...")
  
  # make it a raster for extraction
  dat_rast <- rast(dat$summary$filename)
  
  print("Extracting points...")
  
  pts <- vect(pts_to_extract)
  dat2 <- extract(dat_rast, pts)
  
  print("Reformatting data...")
  
  ret <- dat2 |>
    mutate(Latitude = pts$Latitude,
           Longitude = pts$Longitude) |>
    tidyr::pivot_longer(-c(ID, Latitude, Longitude)) |>
    mutate(name = gsub("_(..)$", "_0\\1", name)) |> #fix no 0 in names for < 100
    mutate(name = gsub("_(.)$", "_00\\1", name)) |> #fix no 0 in names for < 10
    group_by(Latitude, Longitude) |>
    arrange(name) |>
    mutate(Date = time(dat_rast)) |>
    left_join(ll_info)
  
  if(plot_check){
    ggplot(dat2a,
         aes(x = Date, y = value, 
             color = Longitude,
             group = ID)) +
    geom_point() +
    geom_line() +
    scale_color_viridis_c(option = "I")
  }
  print("Writing out data...")
  
  readr::write_csv(ret, file = paste0(out_path, year, ".csv"))

  print("Done. Purging cache.")
  cache_delete(dat)
  
  Sys.sleep(sleep)
  
  #return
  ret
  
  
}


# Get the data by iterating over the years for the function
#note, 2002-2003 are for schoodic
purrr::walk(c(2002, 2003, seq(2013, 2022)), get_timeseries)


