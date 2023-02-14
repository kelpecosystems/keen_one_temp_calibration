#'------------------------------------------------------
#' Read my hobo logger files
#'------------------------------------------------------

library(readr)
library(stringr)
library(dplyr)
library(purrr)

files <- list.files("raw_data/byrnes/hobo_files", recursive = TRUE, full.names = TRUE)
files <- str_subset(files, "\\.csv")

#function for dumb lubritate/mutate thing I could not solve
get_date <- function(dt, dt2){
  if(is.na(dt[1])){
    return(dt2)
  }
  
  return(dt)
}

#function to read and clean hobo data as needed
read_hobo <- function(afile){
  hob <- read_csv(afile, skip = 1) %>%
    select(-`#`)
  
  #proper names
  names(hob)[1] <- "datetime"
  names(hob)[2] <- "temp_c"
  
  #get the logger number
  logger <- names(hob)[3] %>%
    str_replace("(.*)(\\d\\d\\d)\\)$", "\\2")
  
  hob <- hob %>%
    mutate(
      #covert date to proper time
      dt = lubridate::mdy_hms(datetime, tz = "EST"),
      dt2 = lubridate::ymd_hms(paste0(20, datetime), tz = "EST"),
      datetime = get_date(dt, dt2),
      
      #get F or C - if > 1e3 measurements > 50, it's in F
      #because, seriously, that's a lot of time > 122F if it's actually C
      what_temp = sum(temp_c>50, na.rm=T)>1e3,
      temp_c = ifelse(what_temp, (temp_c-32)*5/9, temp_c),
  
      #add ID info
      logger_id = logger,
      
      deployment_year = lubridate::year(datetime)[1]) %>% 
    select(logger_id, deployment_year, datetime, temp_c)
  
  hob
}

#read in the hobo temp files
temps <- map_df(files, read_hobo)

#write out
write_csv(temps, "raw_data/byrnes/processed_data/combined_hobo_data.csv")
