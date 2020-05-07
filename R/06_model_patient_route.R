# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(devtools)
library(ggplot2)
library(sf)
library(lwgeom)
library(tmap)    
library(raster)
library(spData)
library(spDataLarge)
library(mapview)
library(ggmap)

# install_github("dkahle/ggmap")
# install.packages("spData")
# install.packages("mapview")
# install_github("Nowosad/spDataLarge")
# install_github("r-spatial/sf")
# install_github("r-spatial/lwgeom")


# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data
# ------------------------------------------------------------------------------
patient_df <- read_tsv("data/patient_data_augmented.tsv")

# Model the data
# ------------------------------------------------------------------------------
#map where we can see either places of infection or route of the patients (Google maps). 
# Coloring regions by age group. 

localizacion="South Korea"
myMap <- get_map(location=localizacion, zoom= 18, source="google", 
                 maptype="roadmap", crop=TRUE)

