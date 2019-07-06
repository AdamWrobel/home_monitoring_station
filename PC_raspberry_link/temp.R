since <- start_minus_one - 19

PM_df_stored %>%   
    filter(as.numeric(substr(date,7,7)) >= 5) %>%
    group_by(measurement, owner) %>% arrange(measurement, owner, desc(date_time)) %>% 
    mutate(level_avg = 1/7 * lag(level,3) + 1/7 * lag(level,2) + 1/7 * lag(level,1) + 1/7 * level +1/7 * lead(level,1) + 1/7 * lead(level,2) + 1/7 * lead(level,3),
           level_avg = ifelse(is.na(level_avg),level,level_avg),
           level_avg = ifelse(level_avg < 0.9 * level & level > 50, level,level_avg)) %>%
    filter(measurement == 'Temperature') %>% mutate(grouping = paste(measurement,owner)) %>%
    ggplot() + geom_line(aes(x = date_time, y = level_avg, colour = measurement, group = grouping),
                         lwd = 1) #+ #facet_wrap(~measurement, scale = 'free')+ 
