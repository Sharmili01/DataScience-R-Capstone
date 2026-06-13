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
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  
  # Test generate_city_weather_bike_data() function
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create `cities_max_bike` with each row containing city location info and max bike prediction
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII) %>%
    slice_max(BIKE_PREDICTION, with_ties = FALSE) %>%
    ungroup()
  
  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown != 'All') {
      # If a specific city was selected, then render a leaflet map with one marker
      # on the map and a popup with DETAILED_LABEL displayed
      filtered_df <- city_weather_bike_df %>% filter(CITY_ASCII == input$city_dropdown)
      
      output$city_bike_map <- renderLeaflet({
        leaflet(filtered_df) %>% 
          addTiles() %>%
          addCircleMarkers(lng = ~LNG, lat = ~LAT, 
                           radius = ~ifelse(BIKE_PREDICTION_LEVEL=='small', 6, ifelse(BIKE_PREDICTION_LEVEL=='medium', 10, 12)),
                           color = ~color_levels(BIKE_PREDICTION_LEVEL),
                           stroke = FALSE, fillOpacity = 0.8,
                           popup = ~DETAILED_LABEL)
      })
      
      # Task 3: Render temperature trend line
      output$temp_line <- renderPlot({
        ggplot(filtered_df, aes(x = FORECASTDATETIME, y = TEMPERATURE)) +
          geom_line(color = "orange", size = 1) +
          geom_point() +
          labs(title = paste("Temperature Trend for", input$city_dropdown),
               x = "Date & Time", y = "Temperature (C)") +
          theme_minimal()
      })
      
      # Task 4: Render interactive bike prediction trend line
      output$bike_line <- renderPlot({
        ggplot(filtered_df, aes(x = FORECASTDATETIME, y = BIKE_PREDICTION)) +
          geom_line(color = "blue", size = 1) +
          geom_point() +
          labs(title = paste("Bike Prediction Trend for", input$city_dropdown),
               x = "Date & Time", y = "Predicted Bikes") +
          theme_minimal()
      })
      
      # Task 4 (Bonus): Capture clicks on the bike prediction plot
      output$bike_date_output <- renderText({
        if (is.null(input$plot_click)) return("Click on the plot above to see exact values.")
        
        paste("Time =", as.character(as.POSIXct(input$plot_click$x, origin = "1970-01-01")), 
              "\nPredicted Bikes =", round(input$plot_click$y))
      })
      
      # Task 5: Render humidity and bike prediction correlation plot using polynomial fit
      output$humidity_pred_chart <- renderPlot({
        ggplot(filtered_df, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          geom_point() +
          geom_smooth(method = "lm", formula = y ~ poly(x, 4), color = "red", se = FALSE) +
          labs(title = paste("Humidity vs Bike Prediction for", input$city_dropdown),
               x = "Humidity (%)", y = "Predicted Bikes") +
          theme_minimal()
      })
      
    } else {
      # If "All" was selected from dropdown, render a leaflet map for all five cities
      output$city_bike_map <- renderLeaflet({
        leaflet(cities_max_bike) %>%
          addTiles() %>%
          addCircleMarkers(lng = ~LNG, lat = ~LAT,
                           radius = ~ifelse(BIKE_PREDICTION_LEVEL == 'small', 6, 
                                            ifelse(BIKE_PREDICTION_LEVEL == 'medium', 10, 12)),
                           color = ~color_levels(BIKE_PREDICTION_LEVEL),
                           stroke = FALSE, fillOpacity = 0.8,
                           popup = ~LABEL)
      })
      
      # Clear the plots if "All" is selected to keep the UI clean
      output$temp_line <- renderPlot({ NULL })
      output$bike_line <- renderPlot({ NULL })
      output$bike_date_output <- renderText({ NULL })
      output$humidity_pred_chart <- renderPlot({ NULL })
    }
  })
})