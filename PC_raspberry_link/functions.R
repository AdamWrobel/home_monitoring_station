library(jsonlite)
library(dplyr)
library(purrr)

drop_att <- function(input){
  input[[1]][c(2:5)]
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


adjust_PM2.5 <- function(input, intercept = 0, B_PM10 = 0.684110){
  PM10_Airly <- input %>% filter(measurement == 'PM 10', owner == 'Airly')
  PM25_Airly <- input %>% filter(measurement == 'PM 2.5', owner == 'Airly')
  PM_vectorized <- 
    PM10_Airly %>% select(date_time, PM10 = level) %>% 
    full_join(PM25_Airly %>% select(PM25 = level, date_time)) %>%
    arrange(date_time) %>%
    mutate(PM10 = ifelse(is.na(PM10), 1/2 * lead(PM10) + 1/2 * lag(PM10),PM10),
           PM25 = ifelse(is.na(PM25), 1/2 * lead(PM25) + 1/2 * lag(PM25),PM25)) %>%
    filter(is.na(PM10) == F,is.na(PM25) == F)
  
  model <- lm(PM25 ~ PM10 + 0,data = PM_vectorized)
  print(summary(model))
  
  PM10_domek <- input %>% filter(measurement == 'PM 10', owner == 'Domek')
  PM25_domek <- PM10_domek %>% mutate(measurement = 'PM 2.5',
                                      level = level * model$coefficients)
  
  output <-
    input %>% mutate(to_drop = ifelse(measurement != 'PM 10' & owner != 'Domek', 1, 0)) %>% 
    filter(to_drop == 0) %>% select(-to_drop) %>% rbind(PM25_domek)
  
  output <-
    input %>% filter(paste(measurement, owner) != 'PM 2.5 Domek') %>% 
    rbind(PM25_domek)
  return(output)
}

library(ggplot2)