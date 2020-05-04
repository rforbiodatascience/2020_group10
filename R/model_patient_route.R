# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(viridis)
library(forcats)
library(ggplot2)
library(maps)
library(mapproj)

#library(ggmap)
#library(sf)
#library(devtools)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data
# ------------------------------------------------------------------------------
patient_df <- read_tsv("data/patient_data_augmented.tsv")

# Wrangle the data
# ------------------------------------------------------------------------------
lon_lan <- patient_df %>% 
  dplyr::select(.,c(longitude, latitude, sex, age_group, province_patient_route, state)) %>% 
  filter(!is.na(c(longitude))) %>% 
  mutate(sex = as_factor(sex)) #%>% 
#  mutate(state = case_when(state == "released" ~ 0,
#                           state == "deceased" ~ 1, 
#                           state == "isolated" ~ 2
#                           )) %>% 
#  mutate(state = as_factor(state)) 

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

new_table <- range_coordinates %>% 
  full_join(cases_number,
            by= c("state","province_patient_route"))

# Set map location and parameters
# ------------------------------------------------------------------------------
south_korea <- map_data("world") %>% filter(region=="South Korea")
south_korea

#Data visualization for state
# ------------------------------------------------------------------------------
sk_state <- ggplot() +
  geom_polygon(data = south_korea, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  theme_void() +
  coord_map() +
  geom_point( data = lon_lan, aes( x=longitude, y=latitude, color= state)) +
  scale_color_viridis(discrete=TRUE, alpha= 0.5, name = "Patient state", labels = c("released", "deceased", "isolated"))

#Data visualization for age_gruop
# ------------------------------------------------------------------------------
sk_age_group <- ggplot() +
  geom_polygon(data= south_korea, aes(x=long, y = lat, group = group), fill="grey", alpha=0.4) +
  theme_void() +
  coord_map() +
  geom_point( data = lon_lan, aes( x=longitude, y=latitude, color= age_group)) +
  scale_color_viridis(option= "magma", discrete=TRUE, alpha= 0.5, name = "Age group") 

#Data visualization by number
# ------------------------------------------------------------------------------
otro <- ggplot() +
  geom_polygon(data= south_korea, aes(x=long, y = lat, group = group), fill="grey", alpha=0.4) +
  theme_void() +
  coord_map() +
  geom_point( data = new_table, aes( y=min_latt,  x=max_lonn, color= state, size= n)) +
  scale_color_viridis(option= "inferno", discrete=TRUE, alpha= 0.7)  +
  scale_alpha_continuous() + 
  scale_size_continuous(range= c(1,15), name= "Number of patients") +
  guides(colour = guide_legend())
otro

# Save the plots 
# ------------------------------------------------------------------------------
ggsave(filename = "results/05_plot_state.png", plot = sk_state)
ggsave(filename = "results/05_plot_age.png", plot = sk_age_group)

# Save the data frame 
# ------------------------------------------------------------------------------
write_tsv(x = lon_lan, path = "data/wrangled_longitude_latitude_df.tsv")
 