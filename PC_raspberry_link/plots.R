PM_plot <- PM_df_stored %>% filter(date >= Sys.Date() -5 ) %>%
    group_by(measurement, owner) %>% arrange(measurement, owner, desc(date_time)) %>% 
    mutate(level_avg = 1/7 * lag(level,3) + 1/7 * lag(level,2) + 1/7 * lag(level,1) + 1/7 * level +1/7 * lead(level,1) + 1/7 * lead(level,2) + 1/7 * lead(level,3),
           level_avg = ifelse(is.na(level_avg),level,level_avg),
           level_avg = ifelse(level_avg < 0.9 * level & level > 50, level,level_avg)) %>%
    filter(!measurement %in% c('PM 1','Humidity','Temperature'),date >= '2018-02-12') %>% mutate(grouping = paste(measurement,owner)) %>%
    ggplot() + geom_line(aes(x = date_time, y = level_avg, colour = measurement, group = grouping, alpha = owner),
                         lwd = 1) + #facet_wrap(~measurement, scale = 'free')+ 
    geom_hline(yintercept=50, linetype="dashed", 
               color = gg_color_hue(2)[1], size=1.5, alpha = 0.5) +
    geom_hline(yintercept=25, linetype="dashed", 
               color = gg_color_hue(2)[2], size=1.5, alpha = 0.5) +
    scale_alpha_manual(values=c(1,0.4,0.2))+
    scale_linetype_manual(values=c(1, 3))+
    xlab('time')
print(PM_plot)



png(paste0('plots_local/PM_from_',Sys.Date() - 5,'_to_',  Sys.Date(),'.png'), 
    width = 1920*2, height = 1080*2, res = 200*2)
PM_plot 
dev.off()

png(paste0('plots/PM_from_last_five_days.png'), 
    width = 1920*2, height = 1080*2, res = 200*2)
PM_plot 
dev.off()



PM10_plot2 <-
    PM_df_stored %>% 
    group_by(measurement, owner) %>% arrange(measurement, owner, desc(date_time)) %>% 
    mutate(level_avg = 1/7 * lag(level,3) + 1/7 * lag(level,2) + 1/7 * lag(level,1) + 
               1/7 * level + 
               1/7 * lead(level,1) + 1/7 * lead(level,2) + 1/7 * lead(level,3),
           level_avg = ifelse(is.na(level_avg),level,level_avg),
           level_avg = ifelse(level_avg < 0.9 * level & level > 50, level,level_avg)) %>%
    #adjust_PM2.5 %>% 
    #filter(date >= as.Date("2018-02-12"), as.numeric(substr(time,1,2)) >= (0-1) ) %>%
    filter(date >= start_minus_one, as.numeric(substr(time,1,2)) >= (0-1) ) %>%
    filter(!measurement %in% c('PM 1','Humidity','Temperature')) %>% mutate(grouping = paste(measurement,owner)) %>%
    ggplot() + geom_line(aes(x = date_time, y = level_avg, colour = measurement, group = grouping, alpha = owner),
                         lwd = 1) + #facet_wrap(~measurement, scale = 'free')+ 
    #geom_hline(yintercept=10, linetype="dotted", 
    #           aes(colour = 'Safe'), size=1.5, alpha = 0.8) +
    geom_hline(yintercept=10, linetype="dotted", 
               color = gg_color_hue(4)[4], size=1.5, alpha = 0.8) +
    #geom_ribbon(aes(x = date_time ,ymin = 15, ymax = 50, alpha = 'Safe'),
    #            fill = gg_color_hue(4)[4]) +
    geom_hline(yintercept=40, linetype="dotted", 
               color = gg_color_hue(4)[4], size=1.5, alpha = 0.8) +
    scale_alpha_manual(values=c(1,0.4))+
    scale_linetype_manual(values=c(1, 3))+
    xlab('time') #+ ylim(0,70)

print(PM10_plot2)

png(paste0('plots_local/PM_since_previous_query_',gsub(':','_',gsub(' ','_',date())),'.png'), 
    width = 1920*2, height = 1080*2, res = 200*2)
PM10_plot2 
dev.off()

png(paste0('plots/PM_since_previous_query.png'), 
    width = 1920*2, height = 1080*2, res = 200*2)
PM10_plot2 
dev.off()



humidity_plot <- PM_df_stored %>% filter(date >= Sys.Date() -5 ) %>%
    group_by(measurement, owner) %>% arrange(measurement, owner, desc(date_time)) %>% 
    mutate(level_avg = 1/7 * lag(level,3) + 1/7 * lag(level,2) + 1/7 * lag(level,1) + 1/7 * level +1/7 * lead(level,1) + 1/7 * lead(level,2) + 1/7 * lead(level,3),
           level_avg = ifelse(is.na(level_avg),level,level_avg),
           level_avg = ifelse(level_avg < 0.9 * level & level > 50, level,level_avg)) %>%
    filter(measurement == 'Humidity') %>% mutate(grouping = paste(measurement,owner)) %>%
    ggplot() + geom_line(aes(x = date_time, y = level_avg, colour = measurement, group = grouping, alpha = owner),
                         lwd = 1) + #facet_wrap(~measurement, scale = 'free')+ 
    geom_hline(yintercept=65, linetype="dashed", 
               color = gg_color_hue(2)[1], size=1.5, alpha = 0.5) +
    geom_hline(yintercept=35, linetype="dashed", 
               color = gg_color_hue(2)[2], size=1.5, alpha = 0.5) +
    scale_alpha_manual(values=c(1,0.4,0.2))+
    scale_linetype_manual(values=c(1, 3))+
    xlab('time') +
    ylim(30,80)
print(humidity_plot)


png(paste0('plots_local/humidity_',Sys.Date() - 5,'_to_',  Sys.Date(),'.png'),
    width = 1920*2, height = 1080*2, res = 200*2)
humidity_plot 
dev.off()

png(paste0('plots/humidity_from_last_five_days.png'),
    width = 1920*2, height = 1080*2, res = 200*2)
humidity_plot 
dev.off()

humidity_plot2 <- PM_df_stored %>%   
    group_by(measurement, owner) %>% arrange(measurement, owner, desc(date_time)) %>% 
    filter(date >= start_minus_one, as.numeric(substr(time,1,2)) >= (0-1) ) %>%
    mutate(level_avg = 1/7 * lag(level,3) + 1/7 * lag(level,2) + 1/7 * lag(level,1) + 1/7 * level +1/7 * lead(level,1) + 1/7 * lead(level,2) + 1/7 * lead(level,3),
           level_avg = ifelse(is.na(level_avg),level,level_avg),
           level_avg = ifelse(level_avg < 0.9 * level & level > 50, level,level_avg)) %>%
    filter(measurement == 'Humidity') %>% mutate(grouping = paste(measurement,owner)) %>%
    ggplot() + geom_line(aes(x = date_time, y = level_avg, colour = measurement, group = grouping, alpha = owner),
                         lwd = 1) + #facet_wrap(~measurement, scale = 'free')+ 
    geom_hline(yintercept=65, linetype="dashed", 
               color = gg_color_hue(2)[1], size=1.5, alpha = 0.5) +
    geom_hline(yintercept=35, linetype="dashed", 
               color = gg_color_hue(2)[2], size=1.5, alpha = 0.5) +
    scale_alpha_manual(values=c(1,0.4,0.2))+
    scale_linetype_manual(values=c(1, 3))+
    xlab('time') +
    ylim(30,100)
print(humidity_plot)


png(paste0('plots_local/humidity_since_previous_query',gsub(':','_',gsub(' ','_',date())),'.png'), 
    width = 1920*2, height = 1080*2, res = 200*2)
humidity_plot2
dev.off()

png(paste0('plots/humidity_since_previous_query.png'), 
    width = 1920*2, height = 1080*2, res = 200*2)
humidity_plot2
dev.off()

