# Clear workspace ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries ------------------------------------------------------------------------------
library(tidyverse)
library(viridis)
library(forcats)
library(maps)
library(mapproj)
library(maptools)
library(ggrepel)
library(rgdal)

#library(ggmap)
#library(sf)
#library(devtools)

# Define functions ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Wrangle data ------------------------------------------------------------------------------
patient_df <- read_tsv("data/patient_data_augmented.tsv")

# Wrangle the data
# ------------------------------------------------------------------------------
lon_lan <- patient_df %>% 
  dplyr::select(.,c(longitude, latitude, province_patient_route, state)) %>% 
  filter(!is.na(c(longitude)))

# Create a range of coordinates per city
range_coordinates <- lon_lan %>% 
  dplyr::select(.,c(province_patient_route, state, longitude, latitude)) %>%
  rename(x= longitude) %>% 
  rename(y= latitude) %>% 
  group_by(province_patient_route, state) %>% 
  summarise(min_latt = min (y), max_lonn = max (x))


cases_number <- lon_lan %>% 
  dplyr::select(.,c(province_patient_route, state)) %>%
  group_by(province_patient_route, state) %>% 
  tally() %>% 
  as_tibble

cases_number <- range_coordinates %>% 
  full_join(cases_number,
            by= c("state","province_patient_route"))

cases_number_ploty <- cases_number %>% 
  mutate(mytext= paste ("Province: ", province_patient_route, "\n",
                        "State: ", state, "\n",
                        "n: ", n, sep= ""))

# Set map location and parameters
# ------------------------------------------------------------------------------
south_korea <- readOGR( 
  dsn= paste0(getwd(),"/data/Igismap/") , 
  layer="South_Korea_Polygon",
  verbose=FALSE
)

spdf_fortified <- tidy(south_korea, region = "name")

# Wrangle location for labels
# ------------------------------------------------------------------------------
spdf_fortified_provs <- spdf_fortified %>% 
  dplyr::select(.,c(id, long, lat)) %>%
  group_by(id) %>% 
  summarise(lat = min (lat), long = max (long))
   

#Data visualization for state
# ------------------------------------------------------------------------------
sk_state <- ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="grey", alpha= 0.4, color="white") +
  theme_void() +
  coord_map() +
  geom_point( data = lon_lan, aes( x=longitude, y=latitude, color= state)) +
  scale_color_viridis(discrete=TRUE, alpha= 0.5, name = "Patient state", labels = c("released", "deceased", "isolated"))

#Data visualization for age_gruop
# ------------------------------------------------------------------------------
sk_age_group <-ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="grey", alpha= 0.4, color="white") +
  theme_void() +
  coord_map() +
  geom_point( data = lon_lan, aes( x=longitude, y=latitude, color= age_group)) +
  scale_color_viridis(option= "magma", discrete=TRUE, alpha= 0.5, name = "Age group") 

#Data visualization by number
# ------------------------------------------------------------------------------
sk_cases_number <- ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="grey", alpha= 0.4, color="white") +
  theme_void() +
  coord_map() +
  geom_point( data = cases_number_ploty, aes( y=min_latt,  x=max_lonn, color= state, size= n)) +
  scale_color_viridis(option= "inferno", discrete=TRUE, alpha= 0.7)  +
  scale_alpha_continuous() + 
  scale_size_continuous(range= c(1,15), name= "number of cases") +
  guides(size= FALSE) +
  theme(legend.position = "bottom", legend.box = "horizontal") 

#Interactive map
interactive_sk_cases <- sk_cases_number %>% 
  ggplotly(tooltip="text")

# Save interactive map 
# ------------------------------------------------------------------------------
htmlwidgets::saveWidget(interactive_sk_cases, "interactive_province_patient_route.html")

# Save the plots 
# ------------------------------------------------------------------------------
ggsave(filename = "results/plot_state.png", plot = sk_state)
ggsave(filename = "results/plot_age.png", plot = sk_age_group)
ggsave(filename = "results/plot_cases_number.png", plot = sk_cases_number)

# Save the data frame 
# ------------------------------------------------------------------------------
write_tsv(x = lon_lan, path = "data/wrangled_longitude_latitude_df.tsv")
write_tsv(x = cases_number, path = "data/wrangled_cases_number_df.tsv")
 