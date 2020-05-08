# Clear workspace ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries ------------------------------------------------------------------------------
library(tidyverse)
library(viridis)
library(forcats)
library(ggplot2)
library(mapproj)
library(rgdal)
library(broom)
library(plotly)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()


# Define functions ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data ------------------------------------------------------------------------------
patient_df <- read_tsv("data/patient_data_augmented.tsv")

# Wrangle the data ------------------------------------------------------------------------------
# Create a range of coordinates per city
range_coordinates <- patient_df %>% 
  dplyr::select(.,c(province_patient_route, state, longitude, latitude)) %>%
  rename(x= longitude) %>% 
  rename(y= latitude) %>% 
  group_by(province_patient_route, state) %>% 
  summarise(lat = min (y), lon = max (x))

# Get a total of released, isolated, diseased by province
cases_number <- patient_df %>% 
  dplyr::select(.,c(province_patient_route, state)) %>%
  group_by(province_patient_route, state) %>% 
  tally() %>% 
  as_tibble

# Join cases_number with range coordinates for each province
cases_number <- range_coordinates %>% 
  full_join(cases_number,
            by= c("state","province_patient_route"))

# Tooltip column for plotly 
cases_number_ploty <- cases_number %>% 
  mutate(mytext= str_c ("Province: ", province_patient_route, "\n",
                        "State: ", state, "\n",
                        "n: ", n, sep = ""))

# Set map location and parameters  ------------------------------------------------------------------------------

# Data for South Korea map
south_korea <- readOGR( 
  dsn = str_c(getwd(),"/data/Igismap/") , 
  layer ="South_Korea_Polygon",
  verbose = FALSE
  )

# Tidy map data
spdf_fortified <- tidy(
  south_korea, 
  region = "name"
  )

#Data visualization by number ------------------------------------------------------------------------------

sk_cases_number <- ggplot() +
  geom_polygon(
    data = spdf_fortified, 
    aes(x = long, y = lat, group = group), 
    fill="grey", 
    alpha= 0.4, 
    color = "white"
    ) +
  theme_void() +
  coord_map() +
  geom_point(
    data = cases_number_ploty, 
    aes( y = lat,  x = lon, color = state, size = n, text = mytext)
    ) +
  scale_color_viridis(
    option = "inferno", 
    discrete = TRUE, 
    alpha= 0.7
    ) +
  scale_alpha_continuous() + 
  scale_size_continuous(
    range= c(1,15), 
    name= "number of cases"
    ) +
  guides(
    size= FALSE
    ) +
  theme(
    legend.position = "bottom", 
    legend.box = "horizontal"
    ) +
  labs(
    title = "South Korea: number of cases per province",
    x = "",
    y = "",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
    )
  

#Interactive map
interactive_sk_cases <- sk_cases_number %>% 
  ggplotly(
    tooltip="text"
    )

# Save interactive map ------------------------------------------------------------------------------
htmlwidgets::saveWidget(
  interactive_sk_cases, 
  "interactive_province_patient_route.html"
  )

# Save the plots ------------------------------------------------------------------------------
ggsave(
  filename = "results/plot_cases_number.png", 
  plot = sk_cases_number
       )

# Save the data frame ------------------------------------------------------------------------------
write_tsv(x = cases_number, 
          path = "data/wrangled_cases_number_df.tsv"
          )
 