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

library(ggplot2)

PM10 <- fromJSON("http://192.168.1.8:8123/api/history/period/2018-02-03T00:00:00+02:00?filter_entity_id=sensor.pm10") 
PM1 <- fromJSON("http://192.168.1.8:8123/api/history/period/2018-02-03T00:00:00+02:00?filter_entity_id=sensor.pm1") 
PM25 <- fromJSON("http://192.168.1.8:8123/api/history/period/2018-02-03T00:00:00+02:00?filter_entity_id=sensor.pm25") 

PM10_Airly <- fromJSON("http://192.168.1.8:8123/api/history/period/2018-02-03T00:00:00+02:00?filter_entity_id=sensor.pm10_airly") 
PM25_Airly <- fromJSON("http://192.168.1.8:8123/api/history/period/2018-02-03T00:00:00+02:00?filter_entity_id=sensor.pm25_airly") 

PM <- list(PM10, PM1, PM25, PM10_Airly,PM25_Airly)

PM_clean <-
PM %>% map(drop_att)


PM_df <-
PM_clean[[1]] %>% mutate(owner = 'Domek') %>%
  rbind(PM_clean[[2]] %>% mutate(owner = 'Domek')) %>%
  rbind(PM_clean[[3]] %>% mutate(owner = 'Domek')) %>%
  rbind(PM_clean[[4]] %>% mutate(owner = 'Airly')) %>%
  rbind(PM_clean[[5]] %>%  mutate(owner = 'Airly')) %>%
  mutate(state = ifelse(state == 'unknown', NA,state),
         state = as.numeric(state),
         date = as.Date(last_changed), 
         time = substr(last_changed,12,19),
         time = paste0(as.numeric(substr(time,1,2))+1,substr(time,3,8)),
         date_time = paste(date, time), 
         date_time = as.POSIXct(date_time)) %>%
  rename(measurement = entity_id, level = state) %>%
  mutate(measurement = gsub('sensor.','',measurement),
         measurement = gsub('_airly','',measurement),
         measurement = gsub('pm25','PM 2.5',measurement),
         measurement = gsub('pm10','PM 10',measurement),
         measurement = gsub('pm1','PM 1',measurement)) %>% filter(is.na(level) == F)

head(PM_df)
PM_df %>% select(measurement, owner) %>% table

save(PM_df, file = paste0('E:/1TB_disk/Dane/Projekty/PM_sensors/Data/PM_df',gsub(':','_',gsub(' ','_',date())),'.Rdata'))

PM_df$owner <- factor(PM_df$owner, levels = c('Domek','Airly'))

PM_plot <- PM_df %>% filter(measurement != 'PM 1') %>% mutate(grouping = paste(measurement,owner)) %>%
 ggplot() + geom_line(aes(x = date_time, y = level, colour = measurement, group = grouping, alpha = owner),
                               lwd = 1.2) + #facet_wrap(~measurement, scale = 'free')+ 
  geom_hline(yintercept=50, linetype="dashed", 
             color = gg_color_hue(2)[1], size=1.5, alpha = 0.5) +
  geom_hline(yintercept=25, linetype="dashed", 
             color = gg_color_hue(2)[2], size=1.5, alpha = 0.5) +
  scale_alpha_manual(values=c(1,0.4))+
  scale_linetype_manual(values=c(1, 3))+
  xlab('time')
print(PM_plot)
       

png(paste0('E:/1TB_disk/Dane/Projekty/PM_sensors/Data/PM',gsub(':','_',gsub(' ','_',date())),'.png'), 
    width = 1920*2, height = 1080*2, res = 200*2)
PM_plot 
dev.off()

# PM10_plot <- 
# PM_df %>% filter(measurement == 'pm10') %>% 
#   ggplot() + geom_line(aes(x = date_time, y = level, colour = owner, group = owner),
#                        lwd = 1.2) + #facet_wrap(~measurement, scale = 'free')+ 
#   geom_hline(yintercept=25, linetype="dashed", 
#              color = gg_color_hue(3)[1], size=1.5, alpha = 0.5) +
#   scale_linetype_manual(values=c(1, 3))+
#   xlab('time')
# print(PM10_plot)
# png(paste0('E:/1TB_disk/Dane/Projekty/PM_sensors/Data/PM10',gsub(':','_',gsub(' ','_',date())),'.png'), width = 1920, height = 1080, res = 200)
# PM10_plot
# dev.off()
