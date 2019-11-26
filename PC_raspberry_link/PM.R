

library(httr)
library(purrr)
library(jsonlite)
library(dplyr)
setwd('E:/1TB_disk/Dane/Projekty/Monitoring_Station')
source('PC_raspberry_link/functions.R')

library(RCurl)
curlSetOpt(timeout = 2000000)


load('Data/PM_data.Rdata')

write.table('started', file = 'Data/working.txt')

last_stored_obs <-
  PM_df_stored %>% filter(date_time == last(date_time)) %>% select(date, time, date_time)
source('PC_raspberry_link/airly_API.R')

PM_df <-  read.csv(text = getURL('http://192.168.1.8/download_csv.php?id=0&period=curr_week&data[]=pm10&data[]=pm2.5', 
       timeout = 20000, header = F))

colnames(PM_df) <- c('date_time','measurement','level')
tail(PM_df %>% filter(measurement == 'pm10'),15)

PM_dfa <-  read.csv(text = getURL('http://192.168.1.8/download_csv.php?id=0&period=prev_week&data[]=pm10&data[]=pm2.5',
                                 timeout = 20000, header = F))

colnames(PM_dfa) <- c('date_time','measurement','level')
tail(PM_dfa %>% filter(measurement == 'pm10'),15)

PM_df <- rbind(PM_dfa,PM_df)

PM_df2 <- PM_df %>% mutate(measurement = gsub('pm2.5','PM 2.5',measurement),
                          measurement = gsub('pm10','PM 10',measurement),
                          date_time = as.character(date_time),
                          date_time = as.POSIXct(date_time) + 60*60*2,
                          owner = 'Domek',
                          date = as.Date(date_time),
                          time = substr(date_time,12,19))
str(PM_df2)
tail(PM_df2)

both <-
PM_df2 %>% rbind_list(Airly_df %>% select(-last_changed,-last_updated))
str(both)
write.table('PM done', file = 'Data/working.txt')
both$owner <- factor(both$owner, levels = c('Domek','Airly'))


humidity <- (fromJSON(paste0(adress,'sensor.dht_sensor_humidity')) %>% as.data.frame)[,-1] %>%
    transmute(date = as.Date(last_changed),
              time = substr(last_changed,12,19),
              owner = "Domek",
              measurement = 'Humidity',
              level = as.numeric(state),
              date_time = paste(date, time),
              date_time = as.POSIXct(date_time) + 60*60*2,
              time = substr(date_time,12,19))

temperature <- (fromJSON(paste0(adress,'sensor.dht_sensor_temperature')) %>% as.data.frame)[,-1] %>%
    transmute(date = as.Date(last_changed),
              time = substr(last_changed,12,19),
              owner = "Domek",
              measurement = 'Temperature',
              level = as.numeric(state),
              date_time = paste(date, time),
              date_time = as.POSIXct(date_time) + 60*60*2,
              time = substr(date_time,12,19)) %>%
    filter(abs(level - lag(level)) < 3 & abs(level - lead(level)) < 3) %>%
    mutate(slot = 1)

if(dim(temperature)[1]>0){
for (i in 2:(dim(temperature)[1]-1)){
    if(abs(temperature[i,'date_time'] - temperature[i-1,'date_time']) > 2* abs(temperature[i,'date_time'] - temperature[i+1,'date_time'])){
    temperature[i,'slot'] <-  temperature[i-1,'slot'] + 1} else{temperature[i,'slot'] <- temperature[i-1,'slot']}
}
temperature<-
temperature %>% group_by(slot,owner,measurement) %>% summarize(level = median(level)) %>% left_join(temperature %>% filter(slot != lead(slot)) %>% select(-level)) %>% ungroup %>% select(-slot)
}
dht11 <- humidity %>% rbind(temperature)

all <- both %>% rbind(dht11)

db_presence_indicator <-
    all %>% left_join(PM_df_stored %>% select(measurement,owner,date_time) %>% mutate(present_in_db = 1))

to_append <- db_presence_indicator %>% filter(is.na(present_in_db))

PM_df_stored <-
  PM_df_stored %>% rbind(to_append %>% select(-present_in_db))
save(PM_df_stored, file = paste0('data/PM_data.Rdata'))

stories <- list(high_humidity = list(dates = c(as.Date('2018-11-07'),as.Date('2018-11-10'))))
stories[['Krakow_pollution']][['dates']]<-c(as.Date('2018-03-05'),as.Date('2018-03-08'))
last_date <- PM_df_stored %>% tail(1) %>% pull(date)

PM_data_relevant <-   PM_df_stored %>% filter(measurement == 'Humidity') %>%
  filter(substr(date_time,1,10) >= stories$high_humidity$dates[1],substr(date_time,1,10) <= stories$high_humidity$dates[2]) %>%
  rbind(PM_df_stored %>% filter(!measurement %in% c('PM 1','Humidity','Temperature')) %>%
            filter(substr(date_time,1,10) >= stories$Krakow_pollution$dates[1],substr(date_time,1,10) <= stories$Krakow_pollution$dates[2])) %>%
  rbind(PM_df_stored %>% filter(date >= last_date - 10))
save(PM_data_relevant, file = paste0('data/PM_data_relevant.Rdata'))

#save(PM_df_stored, file = paste0('data/PM_df',gsub(':','_',gsub(' ','_',date())),'.Rdata'))
write.table('Finalized', file = 'Data/working.txt')
