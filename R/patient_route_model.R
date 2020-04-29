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
# ggplot figure of a map where we can see either places of infection or route of the patients (Google maps). 
# Coloring regions by age group. 

# Add fill layer to nz shape
tm_shape(world) +
  tm_fill() 
