# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(city_weather_bike_df)
  return(city_weather_bike_df)
}


GetCol_BikeLvl <- function(lvl){
  Col='black'; 
  if(lvl=='small'){
    Col='green'; 
  } else if(lvl=='medium'){
    Col='yellow';
  } else if(lvl=='large'){
    Col='red';
  } else {
    Col='black';
  }
  return(Col)
}

GetRad_BikeLvl <- function(lvl){
  Rad=4; 
  if(lvl=='small'){
    Rad=6; 
  } else if(lvl=='medium'){
    Rad=10;
  } else if(lvl=='large'){
    Rad=12;
  } else {
    Rad=4;
  }
  return(Rad)
}

GetLvl_BikePred <- function(Pred){
  Lvl='small'; 
  if(Pred>=0 & Pred<=1000){
    Lvl='small'
  } else if(Pred>1000 & Pred<=3000){
    Lvl='medium'
  } else {
    Lvl='large'
  }
  return(Lvl)
}


# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  
  # Test generate_city_weather_bike_data() function
  city_weather_bike_df <- test_weather_data_generation()

  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>% group_by(CITY_ASCII, LAT, LNG) %>% 
      summarize(MAXBIKE=max(BIKE_PREDICTION), 
                MAXBIKELEVEL = GetLvl_BikePred(max(BIKE_PREDICTION)) )
  
  cities_max_bike <- cities_max_bike %>% mutate(
                        COLOR=GetCol_BikeLvl(MAXBIKELEVEL),
                        RADIUS=GetRad_BikeLvl(MAXBIKELEVEL))
  
  #Set Label / Detailed-Label values to the first value of predictions at Max for each city
  Lbl_atMaxPred <- c()
  DetLbl_atMaxPred <- c()
  for(city in cities_max_bike$CITY_ASCII){
    df_tmp <- cities_max_bike %>% filter(CITY_ASCII==city) %>% select(MAXBIKE)
    MaxPred <- df_tmp[[1,3]]
    
    df_tmp2 = city_weather_bike_df %>% filter(CITY_ASCII==city, BIKE_PREDICTION==MaxPred) %>%
                select(LABEL, DETAILED_LABEL)
    Lbl_atMaxPred    <- c( Lbl_atMaxPred, df_tmp2[[1,1]] )
    DetLbl_atMaxPred <- c( DetLbl_atMaxPred, df_tmp2[[1,2]] )
  }
  cities_max_bike$MAXLABEL    <- Lbl_atMaxPred
  cities_max_bike$MAXDETLABEL <- DetLbl_atMaxPred
  #print(cities_max_bike)
  
  
  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    
    if(input$city_dropdown != 'All') {
      #map/info for selected city
      df_thiscity <- cities_max_bike %>% filter(CITY_ASCII==input$city_dropdown)
      output$city_bike_map <- renderLeaflet({
        leaflet() %>% addTiles() %>% 
          addMarkers(lat = df_thiscity$LAT, 
                     lng = df_thiscity$LNG, 
                     popup = df_thiscity$MAXDETLABEL)
      })
      
      
      #temperature plot
      df2_thiscity <- city_weather_bike_df %>% filter(CITY_ASCII==input$city_dropdown) %>%
                      mutate(DTFORECAST = as.POSIXct(FORECASTDATETIME, tz='UTC'))
      
      output$temp_line <- renderPlot({
        ggplot(data=df2_thiscity, aes(x=DTFORECAST)) +
          geom_line( aes(y=TEMPERATURE), color='maroon', linetype='dashed') +
          geom_point( aes(y=TEMPERATURE) ) +
          geom_text(aes(y=TEMPERATURE, label=paste(' ',round(TEMPERATURE,1),'C ')), 
                    check_overlap = TRUE, hjust='inward') +
          labs(x='Date', y='Temperature (C-deg)', title='Temperature Chart') +
          theme_bw()
      })
      
      #bike prediction plot
      output$bike_line <- renderPlot({
        ggplot(data=df2_thiscity, aes(x=DTFORECAST)) +
          geom_line( aes(y=BIKE_PREDICTION), linetype='longdash', color='blue') +
          geom_point( aes(y=BIKE_PREDICTION) ) +
          geom_text( aes(y=BIKE_PREDICTION, label=paste(' ',BIKE_PREDICTION,' ')),
                     check_overlap = TRUE, hjust='inward')+
          labs(x='Date', y='Expected Bike Count', title='Bike Demand Prediction with Temperature')+
          theme_bw()
      })
      
      #panel for text output
      output$bike_date_output <- renderText({
        thetime<- input$plot_click$x
        if(!is.null(thetime)){ 
          thetime<-as.POSIXct(thetime, origin="1970-01-01")
        }
        paste('Date, Time :', thetime,
              '\nPredicted Bike Demand:', round(input$plot_click$y+1e-6));
      })
      
      
      #bike trend with humidity
      output$humidity_pred_chart <- renderPlot({
        ggplot(data=df2_thiscity, aes(x=HUMIDITY, y=BIKE_PREDICTION)) +
          geom_point() +
          geom_smooth(method='lm', formula=y ~ poly(x, 4), col='red') +
          labs(x='Humidity (%)', y='Expected Bike Count', title='Bike Demand Prediction with Humidity')+
          theme_bw()
      })
    }
    else {
      output$city_bike_map <- renderLeaflet({
        leaflet() %>% addTiles() %>% 
          addCircleMarkers(lat = cities_max_bike$LAT, 
                           lng = cities_max_bike$LNG, 
                           radius = cities_max_bike$RADIUS,
                           color = cities_max_bike$COLOR,
                           popup = cities_max_bike$MAXLABEL)
      })
    } 

  })


})
