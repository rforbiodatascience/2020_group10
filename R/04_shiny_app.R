# Clear workspace ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries ------------------------------------------------------------------------------
library("tidyverse")
library("shiny")
library("geosphere")
library("leaflet")

# Define functions ------------------------------------------------------------------------------
source(file = "../R/99_project_functions.R")

# Load data ------------------------------------------------------------------------------
patient_df <- read_tsv(file = "../data/patient_data_augmented.tsv")

# Shiny App ------------------------------------------------------------------------------
options(shiny.sanitize.errors = TRUE)

# Server sided code
server <- shinyServer(function(input, output, session){
  output$Map <- renderLeaflet({
    # Load input from sidepanel
    input_date <- input %>% 
      pluck("location_date")
    
    input_latitude <- input %>% 
      pluck("latitude")
    
    input_longitude <- input %>% 
      pluck("longitude")  
    
    # Wrangle data
    distances <- patient_df %>% 
      select(date, latitude, longitude) %>% 
      filter(date == as.Date(input_date)) %>%
      mutate(name = "Patient", 
             col = "red") %>% 
      drop_na()
  
    pins_to_map <- distances %>% 
      add_row(date = as.Date(input_date), 
              latitude = input_latitude, 
              longitude = input_longitude, 
              col = "blue", 
              name = "you")
    
    # Visualization
    pins_to_map %>%
      leaflet()  %>%
      addTiles()  %>%
      addMarkers(popup=pins_to_map$name)  %>%
      addCircleMarkers(color = pins_to_map$col) %>%
      addLegend(labels = c("patient","you"), 
                colors = c("red", "blue"), title = "Risk overview") 
    
  })
  # Distance text
  output$distText <- renderText({
    
    # Inputs from sidepanel
    input_date <- input %>% 
      pluck("location_date")
    
    input_latitude <- input %>% 
      pluck("latitude")
    
    input_longitude <- input %>% 
      pluck("longitude")
    
    # Filter by selected date
    distances <- patient_df %>% 
      select(date, latitude, longitude) %>% 
      filter(date == as.Date(input_date)) %>%
      drop_na()
    
    
    # Calculate distance and convert meters to km's
    distances <- distances %>%
      rowwise() %>%
      mutate(distance = distHaversine(
        c(longitude, latitude),
        c(input_longitude, input_latitude)
      )/1000)
    
    
    # Select the closest distance
    nearest_distance <- distances %>%
      arrange(distance) %>%
      rowwise() %>%
      select(distance) %>%
      min()
    
    # Configure risk estimate
    if (nearest_distance < 1) {
      estimate <- "You'd better get tested."
    }
    else if (nearest_distance < 2) {
      estimate <- "You're probably safe."
    }
    else {
      estimate <- "No need to worry."
    }
    
    # Display message  
    paste("You have been", round(nearest_distance, 1), 
          " km from a potential risk of COVID-19 exposure.", estimate)
    
  })
})

# Client sided code

ui <- shinyUI(pageWithSidebar(
  
    # Application title
    headerPanel("South Korean COVID-19 Exposure Risk"),
    
    # Side bar panel
    sidebarPanel(
      # Date of being at the location
      dateInput("location_date", h3("Date at location"), value="2020-01-20"),
      
      # Input altitude and longitude parameter
      numericInput("latitude", h3("Latitude"), value=37.31),
      numericInput("longitude", h3("Longitude"), value=127.1)
    ),
    mainPanel(
      h3(textOutput("distText")),
      leafletOutput("Map", height = 500),
      
    )
  )
)

# Run app
shinyApp(ui = ui, server = server)
