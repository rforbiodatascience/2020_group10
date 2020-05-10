# Clear workspace ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries ------------------------------------------------------------------------------
library(tidyverse)
library(viridis)
library(rgdal)
library(plotly)
library(maptools)
library(gpclib)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

# Define functions ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data ------------------------------------------------------------------------------
patient_df <- read_tsv("data/patient_data_augmented.tsv")

# Wrangle the data ------------------------------------------------------------------------------

# Create a range of coordinates per city
range_coordinates <- patient_df %>%
  select(., c(province_patient_route, state, longitude, latitude)) %>%
  rename(x = longitude) %>%
  rename(y = latitude) %>%
  group_by(province_patient_route, state) %>%
  summarise(min_latt = min(y), max_lonn = max(x))

# Get a total of released, isolated, diseased by province
cases_number <- patient_df %>%
  select(., c(province_patient_route, state)) %>%
  group_by(province_patient_route, state) %>%
  tally() %>%
  as_tibble()

# Join cases_number with range coordinates for each province
cases_number <- range_coordinates %>%
  full_join(cases_number,
            by = c("state", "province_patient_route")
  )

# Tooltip column for plotly
cases_number_ploty <- cases_number %>%
  mutate(mytext = str_c("Province: ", province_patient_route, "\n",
                        "State: ", state, "\n",
                        "n: ", n,
                        sep = ""
  ))

# Set map location and parameters  ------------------------------------------------------------------------------

# Data for South Korea map
south_korea <- readOGR(
  dsn = str_c(getwd(), "/_raw/Igismap/"),
  layer = "South_Korea_Polygon",
  verbose = FALSE
)

# Tidy map data
spdf_fortified <- broom::tidy(
  south_korea,
  region = "name"
)

# Data visualization by number ------------------------------------------------------------------------------

sk_cases_number <- ggplot() +
  geom_polygon(
    data = spdf_fortified,
    aes(x = long, y = lat, group = group),
    fill = "grey",
    alpha = 0.4,
    color = "white"
  ) +
  theme_void() +
  coord_map() +
  geom_point(
    data = cases_number_ploty,
    aes(y = min_latt, x = max_lonn, color = state, size = n, text = mytext)
  ) +
  scale_color_viridis(
    option = "inferno",
    discrete = TRUE,
    alpha = 0.7
  ) +
  scale_alpha_continuous() +
  scale_size_continuous(
    range = c(1, 15),
    name = "number of cases"
  ) +
  labs(
    title = "South Korea: number of cases per province",
    x = "",
    y = "",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  )


# Interactive map
interactive_sk_cases <- sk_cases_number %>%
  ggplotly(
    tooltip = "text"
  )

# Save interactive map ------------------------------------------------------------------------------
#htmlwidgets::saveWidget(
#  interactive_sk_cases,
#  "results/interactive_province_patient_route.html"
#)

# Save the plots ------------------------------------------------------------------------------
ggsave(
  filename = "results/07_cases_number.png",
  plot = sk_cases_number,
  height = 8,
  width = 10
)

# Save the data frame ------------------------------------------------------------------------------
write_tsv(
  x = cases_number,
  path = "data/wrangled_cases_number_df.tsv"
)

# Detach external packages ---------------------------------------------------------------------
detach("package:maptools", unload=TRUE)
detach("package:rgdal", unload=TRUE)
detach("package:plotly", unload=TRUE)
detach("package:gpclib", unload=TRUE)
