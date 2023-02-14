#'------------------------------------------------------
#' Merge deployment logs with temp loggers
#'------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
setwd(here::here())

#load in files
info <- read_csv("raw_data/byrnes/processed_data/logger_history.csv")  %>%
  filter(!is.na(Year)) |>
  filter(!is.na(Site))

hobo <- read_csv("raw_data/byrnes/processed_data/combined_hobo_data.csv") %>%
  filter(temp_c < 25) #this is preposterous 

#rotate info file
info_wide <- info %>% 
  #just the data we want
  select(Site, Logger, `Logger ID`, Month, Day, Year) %>%
  mutate(Logger = stringr::str_remove_all(Logger, " .*$")) %>%
  
  #make sure deployed/retrieved matches
  group_by(Site, `Logger ID`) %>%
  arrange(Year) %>%
  mutate(dy = ifelse(Logger == "Deployed", Year, NA)) %>%
  fill(dy) %>% 
  ungroup() %>%
  arrange(Site, `Logger ID`, dy) %>%
  #pivot wider
  pivot_wider(names_from = Logger,
              values_from = c(Month, Day, Year)) %>%
  rename(logger_id = `Logger ID`, 
         deployment_year = `Year_Deployed`) %>%
  select(-dy) #don't need this identifier any more

#combine
hobo_joined <- left_join(hobo, 
                         info_wide) %>%
  rename(`Year_Deployed` = deployment_year) %>%
  mutate(start_cut = paste(Month_Deployed, 
                           Day_Deployed+3, 
                           Year_Deployed, sep = "-"),
         end_cut = paste(Month_Retrieved, 
                         Day_Retrieved-3, 
                         Year_Retrieved, sep = "-")) %>%
  mutate(start_cut = mdy(start_cut),
         end_cut = mdy(end_cut))

# filter loggers not from Appledore - but, check in the morning for deployed
# loggers or retreived loggers with no match in logfiles, but match in hobos
hobo_joined <- hobo_joined %>%
  filter(!is.na(Site))

#check
hobo_check <- hobo_joined %>%
  # filter(!is.na(end_cut)) %>% #not picked up - lost!
  group_by(Site, logger_id, Year_Deployed) %>%
  summarize(
    has_deployed = !is.na(start_cut)[1],
    has_deployed_year = !is.na(Year_Deployed)[1],
    has_retreived = !is.na(end_cut)[1],
    has_retreived_year = !is.na(Year_Retrieved)[1],
    Year_Retrieved = Year_Retrieved[1],
    Year_Deployed = Year_Deployed[1]
  ) %>%
  ungroup()




#trim on start/end
hobo_joined_trimmed <- hobo_joined %>%
  # filter(!is.na(end_cut)) %>% #not picked up - lost!
  group_by(Site, logger_id, Year_Deployed) %>%
  # filter(datetime > start_cut) %>%
  #  filter(datetime < end_cut) %>%
  ungroup()


#check
hobo_daily <- hobo_joined_trimmed %>% 
  #  filter(logger_id == 218)
  group_by(Site, month(datetime), day(datetime), year(datetime)) %>%
  summarize(datetime = datetime[1],
            Year_Deployed = Year_Deployed[1],
            temp_c = mean(temp_c, na.rm=TRUE)) %>% ungroup()

library(ggplot2)
ggplot(hobo_daily,
       aes(x = datetime, y = temp_c, color = factor(Year_Deployed))) +
  geom_point() +
  facet_wrap(~Site)

ggplot(hobo_joined_trimmed %>% filter(logger_id == 218),
       aes(x = datetime, y = temp_c)) +
  geom_line()


#write
write_csv(hobo_joined_trimmed, "raw_data/byrnes/processed_data/merged_hobo_data_with_sites.csv")
