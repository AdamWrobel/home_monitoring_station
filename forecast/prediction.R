
PM10_vec <- 
df_wide %>% filter(!is.na(Domek_PM_10)) %>% pull(Domek_PM_10)

library(forecast)
model <- 
PM10_vec %>% 
    auto.arima()
plot(forecast(model,h=20))
plot(x = 1:200,y = PM10_vec %>% tail(200), type = 'l', xlim = c(0,300))
lines(x = 201:250, y = forecast(model,h=50)$mean, col = 'blue')
#lines(x = 201:221, y = forecast(model,h=20)[2], col = 'red', lty = 2)
#lines(x = 201:221, y = forecast(model,h=20)[3], col = 'red', lty = 2)
