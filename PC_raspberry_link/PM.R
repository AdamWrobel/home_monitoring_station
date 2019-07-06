

library(httr)
library(purrr)
library(jsonlite)
library(dplyr)
setwd('E:/1TB_disk/Dane/Projekty/PM_sensors/')
source('R/functions.R')

library(RCurl)
curlSetOpt(timeout = 2000000)


load('Data/PM_data.Rdata')

last_stored_obs <-
  PM_df_stored %>% filter(date_time == max(date_time)) %>% select(date, time, date_time)

ip <- 'http://192.168.1.8:8123/api/history/period/'
start <- paste0(last_stored_obs$date[1],'T00:00:00+00:00')
day <- format(Sys.time(), "%d")
day <- format(Sys.time(), "%Y-%m-%d")

day_plus_one <- paste0(substr(day,1,1), as.numeric(substr(day,2,2)) + 2)
if(nchar(day_plus_one) > 2){day_plus_one <- substr(day_plus_one,2,3)}
as.Date(day) + 1

day_minus_one <- as.Date(day) - 1
#if(nchar(day_plus_one) < 2){day_plus_one <- substr(day_plus_one,2,3)}

start_minus_one <- paste0(day_minus_one,'T00:00:00+00:00')

end <- paste0(format(Sys.time(), "%Y-%m-"),day_plus_one)
end <- '2020-12-31'

adress <- paste0(ip,start_minus_one,"?end_time=",end,'T00%3A00%3A00%2B02%3A00','&filter_entity_id=')

PM10_Airly <- fromJSON(paste0(adress,'sensor.pm10_airly'))
PM25_Airly <- fromJSON(paste0(adress,'sensor.pm25_airly'))
temp_Airly <- fromJSON(paste0(adress,'sensor.temperature'))
hum_Airly <- fromJSON(paste0(adress,'sensor.humidity'))



Airly <- list(PM10_Airly,PM25_Airly, temp_Airly, hum_Airly)

Airly_clean <-
  Airly %>% map(drop_att)

Airly_df <-
  Airly_clean[[1]] %>% mutate(owner = 'Airly') %>%
  rbind(Airly_clean[[2]] %>% mutate(owner = 'Airly')) %>%
  rbind(Airly_clean[[3]] %>% mutate(owner = 'Airly')) %>%
  rbind(Airly_clean[[4]] %>% mutate(owner = 'Airly')) %>%
  mutate(state = ifelse(state == 'unknown', NA,state),
         state = as.numeric(state),
         date = as.Date(last_changed), 
         time = substr(last_changed,12,19),
         date_time = paste(date, time), 
         date_time = as.POSIXct(date_time) + 60*60*1) %>%
  rename(measurement = entity_id, level = state) %>%
  mutate(measurement = gsub('sensor.','',measurement),
         measurement = gsub('_airly','',measurement),
         measurement = gsub('pm25','PM 2.5',measurement),
         measurement = gsub('pm10','PM 10',measurement),
         measurement = gsub('pm1','PM 1',measurement),
         measurement = gsub('temperature','Temperature',measurement),
         measurement = gsub('humidity','Humidity',measurement)) %>% filter(is.na(level) == F)


#PM_df <- read.csv('http://192.168.1.8/download_csv.php?id=0&period=curr_day&data[]=pm10&data[]=pm2.5', 
#                 header = F)
PM_df <-  read.csv(text = getURL('http://192.168.1.8/download_csv.php?id=0&period=curr_day&data[]=pm10&data[]=pm2.5', 
       timeout = 20000, header = F))

colnames(PM_df) <- c('date_time','measurement','level')
tail(PM_df %>% filter(measurement == 'pm10'),15)

#PM_dfa<- read.csv('http://192.168.1.8/download_csv.php?id=0&period=prev_day&data[]=pm10&data[]=pm2.5',
#         header = F)
PM_dfa <-  read.csv(text = getURL('http://192.168.1.8/download_csv.php?id=0&period=prev_day&data[]=pm10&data[]=pm2.5',
                                 timeout = 20000, header = F))

colnames(PM_dfa) <- c('date_time','measurement','level')
tail(PM_dfa %>% filter(measurement == 'pm10'),15)

PM_df <- rbind(PM_dfa,PM_df)

PM_df2 <- PM_df %>% mutate(measurement = gsub('pm2.5','PM 2.5',measurement),
                          measurement = gsub('pm10','PM 10',measurement),
                          date_time = as.character(date_time),
                          date_time = as.POSIXct(date_time) + 60*60*1,
                          owner = 'Domek',
                          date = as.Date(date_time),
                          time = substr(date_time,12,19))
str(PM_df2)
tail(PM_df2)

both <-
PM_df2 %>% rbind_list(Airly_df %>% select(-last_changed,-last_updated))
str(both)

both$owner <- factor(both$owner, levels = c('Domek','Airly'))


humidity <- (fromJSON(paste0(adress,'sensor.dht_sensor_humidity')) %>% as.data.frame)[,-1] %>%
    transmute(date = as.Date(last_changed),
              time = substr(last_changed,12,19),
              owner = "Domek",
              measurement = 'Humidity',
              level = as.numeric(state),
              date_time = paste(date, time),
              date_time = as.POSIXct(date_time) + 60*60*1)

temperature <- (fromJSON(paste0(adress,'sensor.dht_sensor_temperature')) %>% as.data.frame)[,-1] %>%
    transmute(date = as.Date(last_changed),
              time = substr(last_changed,12,19),
              owner = "Domek",
              measurement = 'Temperature',
              level = as.numeric(state),
              date_time = paste(date, time),
              date_time = as.POSIXct(date_time) + 60*60*1) %>%
    filter(abs(level - lag(level)) < 3 & abs(level - lead(level)) < 3) %>%
    mutate(slot = 1)

for (i in 2:(dim(temperature)[1]-1)){
    if(abs(temperature[i,'date_time'] - temperature[i-1,'date_time']) > 2* abs(temperature[i,'date_time'] - temperature[i+1,'date_time'])){
    temperature[i,'slot'] <-  temperature[i-1,'slot'] + 1} else{temperature[i,'slot'] <- temperature[i-1,'slot']}
}
temperature<-
temperature %>% group_by(slot,owner,measurement) %>% summarize(level = median(level)) %>% left_join(temperature %>% filter(slot != lead(slot)) %>% select(-level)) %>% ungroup %>% select(-slot)

dht11 <- humidity %>% rbind(temperature)

all <- both %>% rbind(dht11)

db_presence_indicator <-
    all %>% left_join(PM_df_stored %>% select(measurement,owner,date_time) %>% mutate(present_in_db = 1))

to_append <- db_presence_indicator %>% filter(is.na(present_in_db))

PM_df_stored <-
  PM_df_stored %>% rbind(to_append %>% select(-present_in_db))




save(PM_df_stored, file = paste0('Data/PM_data.Rdata'))
save(PM_df_stored, file = paste0('Data/PM_df',gsub(':','_',gsub(' ','_',date())),'.Rdata'))

