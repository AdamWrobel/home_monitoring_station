
ip <- 'http://192.168.1.8:8123/api/history/period/'
start <- paste0(last_stored_obs$date[1],'T00:00:00+00:00')
day <- format(Sys.time(), "%d")
day <- format(Sys.time(), "%Y-%m-%d")

day_plus_one <- paste0(substr(day,1,1), as.numeric(substr(day,2,2)) + 2)
if(nchar(day_plus_one) > 2){day_plus_one <- substr(day_plus_one,2,3)}
as.Date(day) + 1

day_minus_one <- as.Date(day) - 14
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
           date_time = as.POSIXct(date_time) + 60*60*2) %>%
    rename(measurement = entity_id, level = state) %>%
    mutate(measurement = gsub('sensor.','',measurement),
           measurement = gsub('_airly','',measurement),
           measurement = gsub('pm25','PM 2.5',measurement),
           measurement = gsub('pm10','PM 10',measurement),
           measurement = gsub('pm1','PM 1',measurement),
           measurement = gsub('temperature','Temperature',measurement),
           measurement = gsub('humidity','Humidity',measurement)) %>% filter(is.na(level) == F)
