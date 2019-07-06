library(httr)
library(purrr)
library(jsonlite)
library(dplyr)
url <- 'https://airapi.airly.eu/v1/nearestSensor/measurements?latitude=50.0860&longitude=19.9112&ma$'

url <- 'https://airapi.airly.eu/v1/nearestSensor/measurements?latitude=50.086&longitude=19.911&maxDistance=1000'
url <- 'https://airapi.airly.eu/v1/sensor/measurements?sensorId=195&historyHours=24&historyResolutionHours=1'
url <- 'https://airapi.airly.eu/v2/measurements/point?lat=52.23611&lng=21.047575'
url <-  'https://airapi.airly.eu/v2/measurements/point?lat=52.23611&lng=21.047575'


airly <- GET(url,add_headers(.headers = c('Accept' = 'application/json',
                                          'apikey' = '06b11ab0938548078644750f4bf452a2')))
                              
airly_mes <- GET(url,add_headers(.headers = c('Accept' = 'application/json',
                                          'apikey' = '06b11ab0938548078644750f4bf452a2',
                                          '/v2/meta/measurements')))
.

195
str(airly)
airly$request$headers
content(airly) %>% combine

airly_raw <- content(airly, 'parsed')

airly_raw %>% str(max.level = 3)
airly_raw$history  %>% str(max.level = 1)
 
airly_df <- airly_raw$history %>% map(unlist) %>% rbind_all
colnames(airly_df) <- gsub('measurements.','',colnames(airly_df))

airly_df <- 
airly_df %>% mutate(fromDateTime = gsub('T',' ',fromDateTime),
                    fromDateTime = gsub('Z','',fromDateTime),
                    fromDateTime = as.POSIXct(fromDateTime),
                    tillDateTime = gsub('T',' ',tillDateTime),
                    tillDateTime = gsub('Z','',tillDateTime),
                    tillDateTime = as.POSIXct(tillDateTime))

airly_df[,3:10] <- airly_df[,3:10] %>% map_df(as.numeric)


