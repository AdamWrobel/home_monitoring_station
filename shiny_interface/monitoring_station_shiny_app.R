#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
#library(jsonlite)
#library(future)
library(repmis)

source_data('https://github.com/AdamWrobel/home_monitoring_station/blob/master/data/PM_data.Rdata?raw=true', stringsAsFactors = F)

PM_df_stored$owner <- as.character(PM_df_stored$owner)
last_date <- PM_df_stored %>% tail(1) %>% pull(date)

# create stories list
stories <- list(high_humidity = list(dates = c(as.Date('2018-11-07'),as.Date('2018-11-10'))))
stories$high_humidity[['plot']] <- 'humidityPlot'
stories$high_humidity[['data']] <- 
  PM_df_stored %>% filter(measurement == 'Humidity') %>%
  filter(substr(date_time,1,10) >= stories$high_humidity$dates[1],substr(date_time,1,10) <= stories$high_humidity$dates[2])

# filter only last five days for small
PM_df_stored <- PM_df_stored %>% filter(date >= last_date - 5)
upper_range_PM <- ceiling((PM_df_stored %>% filter(measurement == 'PM 10') %>% pull(level) %>% max)/100)*100
owners <- PM_df_stored$owner %>% unique


gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Home monitoring station"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Startingdate",
                     "Dates range:",
                     min = as.Date(last_date-5),
                     max = as.Date(last_date),
                     #value = as.Date(substr(Sys.time(),1,10))),
                     #min = as.Date("2019-07-02"),
                     #max = as.Date("2018-07-06"),
                     #value = c(as.Date("2019-07-02"),as.Date("2018-07-06"))
                     value = c(as.Date(last_date - 1),as.Date(last_date))
                     ),
         checkboxGroupInput("sources", "Data sources (Domek - inhouse, Airly - outside):",
                            owners, selected = owners),
         checkboxGroupInput("FixPM", label = 'Options',
                            'Fix scale of PM plot', selected = NA),
         sliderInput("MaxPM", label = 'Upper bound for PM plot if fix scale ticked', min = 10, 
                     max = upper_range_PM, value = c(upper_range_PM), step = 10),#,
         checkboxGroupInput("Stories", label = 'Stories',
                            c('Krakow in Winter 2017', 'Warsaw in Winter 2018', 'Baking Bread', 'Huge humidity in new apartment', 'Cooling down the apartment in hot weeks of Summer 2019'), selected = NULL)
        #tags$head(tags$script(src = "message-handler.js")),
        #actionButton("query", "Query Raspberry")
        ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("humidityPlot"),
         plotOutput("pmPlot"),
         plotOutput("tempPlot")
      )
   )
)

# Define server logic 
server <- function(input, output, session) {
    #executed <- 0
    #observeEvent(input$query, {
    #    future({source('R/PM.R'); executed <- 1})
    #    if(executed == 1) {session$reload()}
    #    #session$sendCustomMessage(type = 'testmessage',
    #    #                          message = 'Query is being exectued')
    #})

  
    output$humidityPlot <- renderPlot({
        #last_night_end <- as.POSIXct(paste0(input$Startingdate[2],' 06:00:00 CET'))
        #last_night_start <- as.POSIXct(paste0(input$Startingdate[2]-1,' 22:00:00 CET'))
        #previous_night_end <- as.POSIXct(paste0(input$Startingdate[2]-1,' 06:00:00 CET'))
        #previous_night_start <- as.POSIXct(paste0(input$Startingdate[2]-2,' 22:00:00 CET'))
        #start <- as.POSIXct(paste0(input$Startingdate[1],' 00:00:00 CET'))
        #end <- min(as.POSIXct(paste0(input$Startingdate[2],' 24:00:00 CET')), Sys.time())
      if(is.null(input$Stories)){story = 'no_story'}else(story=input$Stories)
      if(story =='Huge humidity in new apartment'){data <- stories$high_humidity$data}else{
        data <- PM_df_stored %>% filter(measurement == 'Humidity') %>% filter(owner %in% input$sources) %>%
          filter(substr(date_time,1,10) >= input$Startingdate[1],substr(date_time,1,10) <= input$Startingdate[2])}
      if(length(input$sources) == 1) {alphas = 1} else{alphas = c(0.4,0.8)}
        data %>%
            group_by(measurement, owner) %>% arrange(measurement, owner, desc(date_time)) %>% 
            mutate(level_avg = 1/7 * lag(level,3) + 1/7 * lag(level,2) + 1/7 * lag(level,1) + 1/7 * level +1/7 * lead(level,1) + 1/7 * lead(level,2) + 1/7 * lead(level,3),
                   level_avg = ifelse(is.na(level_avg),level,level_avg),
                   level_avg = ifelse(level_avg < 0.9 * level & level > 50, level,level_avg)) %>%
             mutate(grouping = paste(measurement,owner)) %>%
            ggplot() + 
            #geom_rect(aes(xmin=max(start,last_night_start), xmax=last_night_end, ymin=30, ymax=100, alpha = 'Night')) +
            #geom_rect(aes(xmin=max(start,previous_night_start), xmax=previous_night_end, ymin=30, ymax=100, alpha = 'Night')) +
            geom_line(aes(x = date_time, y = level_avg, colour = measurement, group = grouping, alpha = owner),
                                 lwd = 1) + #facet_wrap(~measurement, scale = 'free')+ 
            geom_hline(yintercept=65, linetype="dotted", 
                       color = gg_color_hue(4)[4], size=1.5, alpha = 0.5) +
            geom_hline(yintercept=35, linetype="dotted", 
                       color = gg_color_hue(4)[4], size=1.5, alpha = 0.5) +
            scale_alpha_manual(values=alphas)+
            scale_linetype_manual(values=c(1, 3))+
            xlab('time') + ylab("Humidity") +
            ylim(20,100) #+
            #xlim(start, end)
        
      
   })
    
    output$pmPlot <- renderPlot({
      validate(need(is.null(input$Stories), message=FALSE))
        #last_night_end <- as.POSIXct(paste0(input$Startingdate[2],' 06:00:00 CET'))
        #last_night_start <- as.POSIXct(paste0(input$Startingdate[2]-1,' 22:00:00 CET'))
        #previous_night_end <- as.POSIXct(paste0(input$Startingdate[2]-1,' 06:00:00 CET'))
        #previous_night_start <- as.POSIXct(paste0(input$Startingdate[2]-2,' 22:00:00 CET'))
        #start <- as.POSIXct(paste0(input$Startingdate[1],' 00:00:00 CET'))
        #end <- min(as.POSIXct(paste0(input$Startingdate[2],' 24:00:00 CET')), Sys.time())
      if(length(input$sources) == 1) {alphas = 1} else{alphas = c(0.4,0.8)}
      if(is.null(input$FixPM)) {MaxPM <- NA} else {MaxPM <- input$MaxPM}
        PM_df_stored %>% filter(!measurement %in% c('PM 1','Humidity','Temperature')) %>% filter(owner %in% input$sources) %>%
            filter(substr(date_time,1,10) >= input$Startingdate[1],substr(date_time,1,10) <= input$Startingdate[2]) %>%
            group_by(measurement, owner) %>% arrange(measurement, owner, desc(date_time)) %>% 
            mutate(level_avg = 1/7 * lag(level,3) + 1/7 * lag(level,2) + 1/7 * lag(level,1) + 
                       1/7 * level + 
                       1/7 * lead(level,1) + 1/7 * lead(level,2) + 1/7 * lead(level,3),
                   level_avg = ifelse(is.na(level_avg),level,level_avg),
                   level_avg = ifelse(level_avg < 0.9 * level & level > 50, level,level_avg)) %>%
            mutate(grouping = paste(measurement,owner)) %>%
            ggplot() + 
            #geom_rect(aes(xmin=max(start,last_night_start), xmax=last_night_end, ymin=0, ymax=Inf, alpha = 'Night')) +
            #geom_rect(aes(xmin=max(start,previous_night_start), xmax=previous_night_end, ymin=0, ymax=Inf, alpha = 'Night')) +
            geom_line(aes(x = date_time, y = level_avg, colour = measurement, group = grouping, alpha = owner),
                                 lwd = 1) + #facet_wrap(~measurement, scale = 'free')+ 
            #geom_hline(yintercept=10, linetype="dotted", 
            #           aes(colour = 'Safe'), size=1.5, alpha = 0.8) +
            geom_hline(yintercept=10, linetype="dotted", 
                       color = gg_color_hue(4)[4], size=1.5, alpha = 0.8) +
            #geom_ribbon(aes(x = date_time ,ymin = 15, ymax = 50, alpha = 'Safe'),
            #            fill = gg_color_hue(4)[4]) +
            geom_hline(yintercept=40, linetype="dotted", 
                       color = gg_color_hue(4)[4], size=1.5, alpha = 0.8) +
            scale_alpha_manual(values=alphas)+
            scale_linetype_manual(values=c(1, 3)) +
            xlab('time') + ylab("Air Pollution") +
            scale_y_continuous(limits = c(0, MaxPM))
            
        
    })
    
    output$tempPlot <- renderPlot({
      validate(need(is.null(input$Stories), message=FALSE))
      if(length(input$sources) == 1) {alphas = 1} else{alphas = c(0.4,0.8)}
        PM_df_stored %>% filter(measurement == 'Temperature') %>% filter(owner %in% input$sources) %>%
            filter(date >= input$Startingdate[1],date <= input$Startingdate[2]) %>%
            group_by(measurement, owner) %>% arrange(measurement, owner, desc(date_time)) %>% 
            mutate(level_avg = 1/7 * lag(level,3) + 1/7 * lag(level,2) + 1/7 * lag(level,1) + 
                 1/7 * level + 
                 1/7 * lead(level,1) + 1/7 * lead(level,2) + 1/7 * lead(level,3),
               level_avg = ifelse(is.na(level_avg),level,level_avg),
               level_avg = ifelse(level_avg < 0.9 * level & level > 50, level,level_avg)) %>%
             mutate(grouping = paste(measurement,owner)) %>%
            ggplot() + geom_line(aes(x = date_time, y = level_avg, colour = measurement, group = grouping, alpha = owner),
                                 lwd = 1) + #facet_wrap(~measurement, scale = 'free')+
            geom_hline(yintercept=25, linetype="dotted",
                       color = gg_color_hue(4)[4], size=1.5, alpha = 0.5) +
            geom_hline(yintercept=21, linetype="dotted",
                       color = gg_color_hue(4)[4], size=1.5, alpha = 0.5) +
            scale_alpha_manual(values=rev(alphas))+
            scale_linetype_manual(values=c(1))+
            xlab('time') + ylab("Temperature")


    })
}

# Run the application 
shinyApp(ui = ui, server = server)

