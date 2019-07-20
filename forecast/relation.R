# modelling attempt

# reformat into wide format
library(tidyr)
PM_df_stored %>% head
df_wide <- PM_df_stored %>% tail(400000) %>% 
    mutate(#date_time = substr(date_time,1,16),
           metric = gsub(pattern = ' ', replace = '_' ,paste(owner,measurement))) %>%
    select(date_time, metric, level) %>% 
    spread(key = metric, value = level)
df_wide %>% head
df_wide %>% View

fill_missing <- function(input, max_NAs = 7){
    input %>% 
        mutate(Airly_Humidity = ifelse(NAs <= max_NAs & is.na(Airly_Humidity), lag(Airly_Humidity),Airly_Humidity),
               Airly_Humidity = ifelse(NAs <= max_NAs & is.na(Airly_Humidity) & is.na(lag(Airly_Humidity)), lead(Airly_Humidity),Airly_Humidity),
               Airly_PM_10 = ifelse(NAs <= max_NAs & is.na(Airly_PM_10), lag(Airly_PM_10),Airly_PM_10),
               Airly_PM_10 = ifelse(NAs <= max_NAs & is.na(Airly_PM_10) & is.na(lag(Airly_PM_10)), lead(Airly_PM_10),Airly_PM_10),
               Airly_PM_2.5 = ifelse(NAs <= max_NAs & is.na(Airly_PM_2.5), lag(Airly_PM_2.5),Airly_PM_2.5),
               Airly_PM_2.5 = ifelse(NAs <= max_NAs & is.na(Airly_PM_2.5) & is.na(lag(Airly_PM_2.5)), lead(Airly_PM_2.5),Airly_PM_2.5),
               Domek_PM_10 = ifelse(NAs <= max_NAs & is.na(Domek_PM_10), lag(Domek_PM_10),Domek_PM_10),
               Domek_PM_10 = ifelse(NAs <= max_NAs & is.na(Domek_PM_10) & is.na(lag(Domek_PM_10)), lead(Domek_PM_10),Domek_PM_10)
        )
}
df_wide2 <-
df_wide %>% 
    # count number of missing variables in a given second/row
    mutate(NAs = 
               is.na(Airly_Humidity) + 
               is.na(Airly_PM_10) + 
               is.na(Airly_PM_2.5)+
               is.na(Airly_Temperature) + 
               is.na(Domek_PM_10) +
               is.na(Domek_PM_2.5) + 
               is.na(Domek_Temperature)) %>%
    # fill missing values for rows with maximum 3 missing variables
    fill_missing %>% fill_missing %>% fill_missing() %>%  fill_missing() %>% filter(NAs <= 5) %>% #%>% filter(Domek_PM_10 > 100)
    mutate(NAs2 = 
               is.na(Airly_Humidity) + 
               is.na(Airly_PM_10) + 
               is.na(Airly_PM_2.5)+
               is.na(Airly_Temperature) + 
               is.na(Domek_PM_10) +
               is.na(Domek_PM_2.5) + 
               is.na(Domek_Temperature)) %>% filter(NAs2 <= 3)

df_wide3 <- df_wide2 %>% mutate(time = substr(date_time, 1,13)) %>% group_by(time) %>%
    summarize(Domek_PM_10 = mean(Domek_PM_10, na.rm = T),
              Airly_Humidity = mean(Airly_Humidity, na.rm = T),
              Airly_PM_10 = mean(Airly_PM_10, na.rm = T),
              Airly_PM_2.5 = mean(Airly_PM_2.5, na.rm = T),
              Airly_Temperature = mean(Airly_Temperature, na.rm = T)) %>% filter(Domek_PM_10 > 10)

model <- lm(Domek_PM_10 ~ Airly_PM_10 + Airly_PM_2.5 + Airly_Temperature + Airly_Humidity,data = df_wide3)
summary(model)

plot(model)

