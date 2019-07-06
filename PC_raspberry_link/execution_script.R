

setwd('E:/1TB_disk/Dane/Projekty/PM_sensors/')
source('R/functions.R')

load('Data/PM_data.Rdata')

last_stored_obs <-
PM_df_stored %>% filter(date_time == max(date_time)) %>% select(date, time, date_time)

ip <- 'http://192.168.1.8:8123/api/history/period/'
start <- paste0(last_stored_obs$date[1],'T00:00:00+00:00')
day <- format(Sys.time(), "%d")
day_plus_one <- paste0(substr(day,1,1), as.numeric(substr(day,2,2)) + 1)
if(nchar(day_plus_one) > 2){day_plus_one <- substr(day_plus_one,2,3)}
end <- paste0(format(Sys.time(), "%Y-%m-"),day_plus_one)

adress <- paste0(ip,start,"?end_time=",end,'T00%3A00%3A00%2B02%3A00','&filter_entity_id=')

PM10 <- fromJSON(paste0(adress,'sensor.pm10'))
PM1 <- fromJSON(paste0(adress,'sensor.pm1'))
PM25 <- fromJSON(paste0(adress,'sensor.pm25'))

PM10s <- fromJSON(paste0(adress,'sensor.pm10s'))
PM25s <- fromJSON(paste0(adress,'sensor.pm25s'))

PM10_Airly <- fromJSON(paste0(adress,'sensor.pm10_airly'))
PM25_Airly <- fromJSON(paste0(adress,'sensor.pm25_airly'))

PM <- list(PM10, PM1, PM25, PM10_Airly,PM25_Airly, PM10s, PM25s)

PM_clean <-
PM %>% map(drop_att)

PM_df <-
PM_clean[[1]] %>% mutate(owner = 'Domek') %>%
  rbind(PM_clean[[2]] %>% mutate(owner = 'Domek')) %>%
  rbind(PM_clean[[3]] %>% mutate(owner = 'Domek')) %>%
  rbind(PM_clean[[4]] %>% mutate(owner = 'Airly')) %>%
  rbind(PM_clean[[5]] %>%  mutate(owner = 'Airly')) %>%
  rbind(PM_clean[[6]] %>%  mutate(owner = 'Domek sp')) %>%
  rbind(PM_clean[[7]] %>%  mutate(owner = 'Domek sp')) %>%
  mutate(state = ifelse(state == 'unknown', NA,state),
         state = as.numeric(state),
         date = as.Date(last_changed), 
         time = substr(last_changed,12,19),
         date_time = paste(date, time), 
         date_time = as.POSIXct(date_time) + 60*60) %>%
  rename(measurement = entity_id, level = state) %>%
  mutate(measurement = gsub('sensor.','',measurement),
         measurement = gsub('_airly','',measurement),
         measurement = gsub('pm25','PM 2.5',measurement),
         measurement = gsub('pm10','PM 10',measurement),
         measurement = gsub('pm1','PM 1',measurement)) %>% filter(is.na(level) == F)

PM_df$owner <- factor(PM_df$owner, levels = c('Domek','Domek sp','Airly'))

head(PM_df)
PM_df %>% select(measurement, owner) %>% table

db_presence_indicator <-
PM_df %>% left_join(PM_df_stored %>% select(measurement,owner,date_time) %>% mutate(present_in_db = 1))

to_append <- db_presence_indicator %>% filter(is.na(present_in_db))

PM_df_stored <-
PM_df_stored %>% rbind(to_append %>% select(-present_in_db))

save(PM_df_stored, file = paste0('Data/PM_data.Rdata'))
save(PM_df_stored, file = paste0('Data/PM_df',gsub(':','_',gsub(' ','_',date())),'.Rdata'))


PM_plot <- PM_df_stored %>% filter(measurement != 'PM 1') %>% mutate(grouping = paste(measurement,owner)) %>%
 ggplot() + geom_line(aes(x = date_time, y = level, colour = measurement, group = grouping, alpha = owner),
                               lwd = 1.2) + #facet_wrap(~measurement, scale = 'free')+ 
  geom_hline(yintercept=50, linetype="dashed", 
             color = gg_color_hue(2)[1], size=1.5, alpha = 0.5) +
  geom_hline(yintercept=25, linetype="dashed", 
             color = gg_color_hue(2)[2], size=1.5, alpha = 0.5) +
  scale_alpha_manual(values=c(1,0.4,0.2))+
  scale_linetype_manual(values=c(1, 3))+
  xlab('time')
print(PM_plot)
       

png(paste0('Plots/full_history/PM',gsub(':','_',gsub(' ','_',date())),'.png'), 
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


PM10_plot2 <-
PM_df_stored %>% #adjust_PM2.5 %>% 
  filter(date >= last_stored_obs$date[1], as.numeric(substr(time,1,2)) >= (0-1) ) %>%
  filter(measurement != 'PM 1') %>% mutate(grouping = paste(measurement,owner)) %>%
  ggplot() + geom_line(aes(x = date_time, y = level, colour = measurement, group = grouping, alpha = owner),
                       lwd = 1.2) + #facet_wrap(~measurement, scale = 'free')+ 
  geom_hline(yintercept=50, linetype="dashed", 
             color = gg_color_hue(2)[1], size=1.5, alpha = 0.5) +
  geom_hline(yintercept=25, linetype="dashed", 
             color = gg_color_hue(2)[2], size=1.5, alpha = 0.5) +
  scale_alpha_manual(values=c(1,0.4,0.4))+
  scale_linetype_manual(values=c(1, 3))+
  xlab('time') #+ ylim(0,70)

print(PM10_plot2)

png(paste0('Plots/since_previous_query/PM_since_previous_query_',gsub(':','_',gsub(' ','_',date())),'.png'), 
    width = 1920*2, height = 1080*2, res = 200*2)
PM10_plot2 
dev.off()

#start_test <- experiments_l[7,'date_time'] %>% as.matrix %>% c
#end_test <- experiments_l[15,'date_time'] %>% as.matrix %>% c

#PM10_plot2 + geom_polygon(aes())

#(aes(x=c(start_test,end_test),y=c(100,100)),fill="#FFFF0044",color=NA)
# PM_df %>% filter(date == '2018-02-04') %>%
#   filter(measurement != 'PM 1') %>% mutate(grouping = paste(measurement,owner)) %>%
#   ggplot() + geom_line(aes(x = date_time, y = level, colour = measurement, group = grouping, alpha = owner),
#                        lwd = 1.2) + #facet_wrap(~measurement, scale = 'free')+ 
#   geom_hline(yintercept=50, linetype="dashed", 
#              color = gg_color_hue(2)[1], size=1.5, alpha = 0.5) +
#   geom_hline(yintercept=25, linetype="dashed", 
#              color = gg_color_hue(2)[2], size=1.5, alpha = 0.5) +
#   scale_alpha_manual(values=c(1,0.4))+
#   scale_linetype_manual(values=c(1, 3))+
#   xlab('time')


