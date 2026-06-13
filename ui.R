require(shiny)
require(leaflet)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bike-sharing demand prediction app"),
  
  # Create a side-bar layout
  sidebarLayout(
    
    # Create a main panel to show cities on a leaflet map
    mainPanel(
      # Task 1: Add a basic max bike prediction overview map
      leafletOutput("city_bike_map", height = 1000)
    ),
    
    # Create a side bar to show detailed plots for a city
    sidebarPanel(
      # Task 2: Add a select input (dropdown list) to select a specific city
      selectInput(inputId = "city_dropdown", 
                  label = "Select City:", 
                  choices = c("All", "Seoul", "New York", "Paris", "London", "Suzhou")),
      
      # Task 3: Add a static temperature trend line
      plotOutput("temp_line"),
      
      # Task 4: Add an interactive bike-sharing demand prediction trend line
      plotOutput("bike_line", click = "plot_click"),
      verbatimTextOutput("bike_date_output"),
      
      # Task 5: Add a static humidity and bike-sharing demand prediction correlation plot
      plotOutput("humidity_pred_chart")
    )
  )
))