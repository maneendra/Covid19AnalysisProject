
library(leaflet)
library(shiny)
library(lubridate)
library(threejs)
library(shinythemes)
library(ggplot2)
library(timevis)
library(timelineS)
library(wordcloud2)

# sourcing external R scripts

source("resources/timeline_covid.R",local= TRUE)
source("resources/timeline_plots_covid.R",local= TRUE)
source("resources/patient_data_analysis.R",local= TRUE)

# Downloading the required datasets

confirmed_url = "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"
recovered_url = "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_recovered_global.csv&filename=time_series_covid19_recovered_global.csv"
deaths_url = "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv"

john_confirmed <- read.csv(textConnection(getURL(confirmed_url)))
john_recovered <- read.csv(textConnection(getURL(recovered_url)))
john_deaths <- read.csv(textConnection(getURL(deaths_url)))

# contains latitude and longitude data
countries_data <- read.csv("resources/countries.csv")
earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"

# preprocessed patient dataset
med <- read.csv("resources/preprocessed_medical_data.csv")

# time.df <- subset(med, !is.na(med$Age)  & !is.na(med$DateOfOnsetSymptoms)
#                   & (!is.na(med$DateOfDeath) | !is.na(med$DateOfDischarge)))
# time.df <- time.df[,c("Age","DateOfOnsetSymptoms","DateOfDeath","DateOfDischarge")]
# 

time.df <- subset(med, !is.na(med$Age)  & !is.na(med$DateOfOnsetSymptoms)
                  & (!is.na(med$DateOfDeath) | !is.na(med$DateOfDischarge)))
time.df <- time.df[,c("Age","DateOfOnsetSymptoms","DateOfDeath","DateOfDischarge")]
time.df$DateOfOnsetSymptoms <-  as.Date(time.df$DateOfOnsetSymptoms,"%Y-%m-%d")
time.df$DateOfDeath <-  as.Date(time.df$DateOfDeath,"%Y-%m-%d")
time.df$DateOfDischarge <-  as.Date(time.df$DateOfDischarge,"%Y-%m-%d")

# globals
daysToPredict <- 20


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  ## Variables and outputs for predictor
  
  
  # reactive element having a generated dataset with predictions
  data_with_prediction <- reactive ({
    
    
    
    ## PROCESSING:
    
    # filterinig out dataseet to select values of only a given country 
    jc <- john_confirmed %>% filter(Country.Region == input$country) %>% select(c(-1,-2,-3,-4)) %>% colSums()
    jr <- john_recovered %>% filter(Country.Region == input$country) %>% select(c(-1,-2,-3,-4)) %>% colSums()
    jd <- john_deaths %>% filter(Country.Region == input$country) %>% select(c(-1,-2,-3,-4)) %>% colSums()
    
    # calculating values for active cases 
    ja <- jc-jr-jd
    
    # Binding and naming all the values into one dataset
    john_selected <- as.data.frame(t(rbind(jc,jr,jd,ja)))
    colnames(john_selected) <- c("confirmed", "recovered", "dead", "active")
    
    
    year = colnames(john_confirmed)
    last_date = tail(year, n=1)
    last_date <- mdy(gsub("X","",last_date))
    last_date <- last_date + lubridate::days(daysToPredict)
    
    # generating a series of dates 
    dates <- seq(as.Date("2020-01-22"), as.Date(last_date), by = "day")
    
    
    # Generating time series data from the data set according to the given parameters 
    data_ts <- ts(as.numeric(unlist(john_selected[input$typeToPredict])), start = c(2020, as.numeric(format(dates[1], "%j"))), frequency = 365)
    
    ## PREDICTION:
    
    # using ARIMA model to generate prediction
    arima_model <- auto.arima(data_ts)
    
    forecast_arima = forecast::forecast(arima_model, h=daysToPredict)
    forecast_arima = round(as.data.frame(forecast_arima)$`Point Forecast`)
    
    ## CLEANING UP
    
    #forecast_arima$cases <- round(forecast_arima$cases, 0)
    
    # adding labels to the datasets which will be used to color the graph later
    data_ts <- as.data.frame(cbind(data_ts,label = "actual"))
    forecast_arima <- as.data.frame(cbind(forecast_arima, label = "predicted"))
    
    # correcting column labels and merging actual data with prediction
    names(data_ts)[1] <- "cases"
    names(forecast_arima)[1] <- "cases"
    data_and_forecast <- rbind(data_ts, forecast_arima)
    
    # Binding dataset with dates which will be used as x asis on the plot
    data_and_forecast <- cbind(data_and_forecast, dates)
    
    names(data_and_forecast)[3] = "date"
    data_and_forecast$cases <-as.integer(data_and_forecast$cases)
    data_and_forecast
    
  })
  
  output$country <- renderText({
    input$country
  })
  
  output$casesType <- renderText({
    ifelse(input$typeToPredict != "dead" ,paste(input$typeToPredict, "cases", sep=" "), "deaths")
  })
  
  output$casesPlot <- renderPlotly({
    
    data_and_forecast <- data_with_prediction()
    
    ## PLOTTING GRAPH:
    
    # configuration for plot layout
    l <- list(
      font = list(
        family = "sans-serif",
        size = 12,
        color = "#000"),
      bgcolor = "#E2E2E2",
      bordercolor = "#FFFFFF",
      borderwidth = 2,
      x = 0,
      y = 100,
      orientation = 'h'
    )
    
    # selecting the plotting area based on input parameters
    slice_start <- nrow(data_and_forecast) - input$graphWindow - (daysToPredict - input$daysToPredict) + 1
    slice_end <- slice_start + input$graphWindow - 1
    plotting_data <- data_and_forecast %>% slice(slice_start:slice_end)
    
    fig <- ggplot(plotting_data)+ 
      geom_line(aes(x = date, y = cases, color =label))+
      geom_point(aes(x = date, y = cases, color =label))+
      scale_color_manual(values=c("blue", "red")) +
      theme_minimal()
    # fig <- plot_ly(type = "scatter", mode = "line+marker", data = plotting_data, x = ~date, y = ~cases,
    #                color = ~label, colors = c("blue", "red")
    # ) 
    fig <- ggplotly(fig) %>% layout(legend= l )
    fig
  })
  
  # table displaynig all the recent predicted vlues 
  output$casesTable <- renderTable({
    data_and_forecast <- data_with_prediction()
    
    # Preprocessing and formating data for readable table display
    data_and_forecast$date <- selected <- format(as.Date(data_and_forecast$date), "%d-%m-%Y")
    # data_and_forecast$label <- ifelse(data_and_forecast$label == 1, "actual", "predicted")
    data_and_forecast$cases <- as.integer(data_and_forecast$cases)
    names(data_and_forecast) <- c("Cases", "Actual/Predicted", "Date")
    
    # slicing only the required values (3 actual, predicted values equal to the number at input)
    slice_start <- nrow(data_and_forecast) - daysToPredict - 2
    slice_end <- slice_start + input$daysToPredict + 2
    data_and_forecast[c(3,1,2)] %>% slice(slice_start:slice_end)
  })
  
  # globe output for predictor tab
  output$globe <- renderGlobe({
    c <- countries_data %>% filter(name == input$country)
    # latitude and longitude values used to add marker and rotate the globe
    lo <- c$longitude
    lor <- ((lo+88.2)*3.1415/180) -0.12
    la <- c$latitude
    lar <- ((la)*3.1415/180)-0.24
    c <- countries_data %>% filter(name == input$country)
    globejs(img=earth, lat = la, long = lo, rotationlat=lar, rotationlong= -lor, 
            pointsize = 2, color="#ff0000", value = 150, bg="#00001e", atmosphere = TRUE)
  })
  
  
  ## Patient data outputs 
  
  # med <- function(){
  #   processed_med()
  # }
  
  output$patient_loc_map <- renderPlotly({patient_loc_map()})
  output$age_dist_plot <- renderPlotly({age_dist_plot()})
  output$age_dist_bl20_plot <- renderPlotly({age_dist_bl20_plot()})
  output$age_dist_20to60_plot <- renderPlotly({age_dist_20to60_plot()})
  output$age_dist_ov60_plot <- renderPlotly({age_dist_ov60_plot()})
  output$days_to_recovery_plot <- renderPlotly({days_to_recovery_plot()})
  output$days_to_death_plot <- renderPlotly({days_to_death_plot()})
  output$symptom_wordcloud <- renderWordcloud2({symptom_wordcloud()})
  output$symptom_piechart <- renderPlotly({symptom_piechart()})
  output$likely_recover_boxplot <- renderPlotly({likely_recover_boxplot()})
  output$unlikely_recover_boxplot <- renderPlotly({unlikely_recover_boxplot()})
  output$chroic_disease_gender_plot <- renderPlotly({chroic_disease_gender_plot()})
  output$chronic_disease_freq_plot <- renderPlotly({chronic_disease_freq_plot()})
  
  ## TIMELINE OUTPUTS 
  
  # Timevis plot
  output$timeline <- renderTimevis({
    timevis(data_covid())
  })
  
  # country timeline plots
  output$confirmed_timeline_plot <- renderPlotly({
    ggplotly(confirmed_timeline_plot())
  })
  output$recovered_timeline_plot <- renderPlotly({
    ggplotly(recovered_timeline_plot())
  })
  output$deaths_timeline_plot <- renderPlotly({
    ggplotly(deaths_timeline_plot())
  })
  
  
  ## Twitter data outputs
  
  output$tweet_plot <- renderUI({
    n <- input$twittercharts
    if(n=="tweet_plot1")
      img(src='top-words-contributed-to-sentiment-1.png', height = '600px')
    else if(n=="tweet_plot2")
      img(src='nrc-analysis-1.png', height = '600px')
    else if(n=="tweet_plot3")
      img(src='word-cloud-by-NRC-emotion-1.png', height = '600px')
    else if(n=="tweet_plot4")
      img(src='sentiments-with-time-1.png', height = '600px')
    else
      img(src='german-word-cloud-2.png', height = '600px')
  })
  
  output$tweet_header <- renderUI({
    n <- input$twittercharts
    if(n=="tweet_plot1")
      tags$h4("Top words that contributed for positive and negative sentiments with the sentiment.")
    else if(n=="tweet_plot2") 
      tags$h4("Tweet count with eight emotions and positive and negative sentiment derived using NRC Sentiment Dictionary.")
    else if(n=="tweet_plot3")
      tags$h4("The Word Cloud generated from English tweets with their emotion category.")
    else if(n=="tweet_plot4")
      tags$h4("Hourly sentiment count change. The graph is based on the tweets retrieved on 29th June 2020, during 21:00 and 23:00 hours.")
    else
      tags$h4("The Word Cloud generated from German tweets.")
  })
  
  output$tweet_summary <- renderUI({
    n <- input$twittercharts
    if(n=="tweet_plot1"){
      tags$div(tags$h4("Here tweets are categorized as positive or negative and then what are the words that have contributed most for their sentiment."),
               tags$br(),
               tags$h4("Based on the chart, it is visible that \"ugh\", \"die\", \"miss\", \"sue\", \"worse\" words have contributed mosly for negative sentiment while \"good\", \"trump\", \"love\", \"hug\", \"wow\" have contributed mostly for positive sentiment.")
      )
    }
    else if(n=="tweet_plot2") 
      tags$div(tags$h4("Here tweets are analyzed for eight emotions \"anger\", \"anticipation\", \"disgust\", \"fear\", \"joy\", \"sadness\", \"surprise\", \"trust\", \"negative\", \"positive\" using NRC sentiment dictionary and bar graph is plotted for each sentiment."),
               tags$br(),
               tags$h4("According to plot, it is evident that people had more positive sentiment towards the pandemic with anticipation and trust.")
      )
    else if(n=="tweet_plot3")
      tags$div(tags$h4("Here a wordcloud is generated for English tweets with their emotion category and it is noticeable that words related to disgust and anger had more weightage among the corpus. "),
      )
    else if(n=="tweet_plot4")
      tags$div(tags$h4("Here the time series graph will show how the positive and negative tweets have changed with time."),
               tags$br(),
               tags$h4("Based on the visualization, it is clearly visible that most of the tweets had positive sentiment and positive tweets have increased with time.")
      )
    else
      tags$div(tags$h4("In the German cloud it is found some interesting relevant words like \"schon\", \"deutschland\", \"pandamie\", \"lockdown\", \"youtube\" etc.")
      )
  })
  
  output$video <- renderUI({
    tags$iframe(src = "https://www.youtube.com/watch?v=IQCKhvJs6-c&feature=youtu.be")
  })
})