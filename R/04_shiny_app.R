# Clear workspace ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries ------------------------------------------------------------------------------
library("tidyverse")
library("shiny")
library("geosphere")

# Define functions ------------------------------------------------------------------------------
source(file = "../R/99_project_functions.R")

# Load data ------------------------------------------------------------------------------
patient_df <- read_tsv(file = "../data/patient_data_augmented.tsv")

# Shiny App ------------------------------------------------------------------------------
options(shiny.sanitize.errors = TRUE)

# Server sided code
server <- shinyServer(function(input, output, session){
  
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
  
    # Display message  
    paste("You have been", round(nearest_distance, 1), " km from a potential risk of COVID-19 exposure")
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
      numericInput("latitude", h3("Latitude"), value=0),
      numericInput("longitude", h3("Longitude"), value=0)
    ),
    mainPanel(
      h3(textOutput("distText"))
    )
  )
)

# Run app
shinyApp(ui = ui, server = server)
