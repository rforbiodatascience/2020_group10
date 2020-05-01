# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load all dataset files of the Korean COVID19 Dataset
# ------------------------------------------------------------------------------

# Get file paths for database table files
dataset_files <- list.files(path = "_raw", full.names = TRUE)

# Load table files and set filename as name in list
dataset_tables <- dataset_files %>%
  setNames(nm = sapply(str_split(., "/"), tail, 1)) %>%
  map(read_csv)

# Case data
# ------------------------------------------------------------------------------
dataset_tables %>%
  pluck("Case.csv") %>% 
  write_tsv(path = "data/case_data.tsv")

# PatientInfo data
# ------------------------------------------------------------------------------
dataset_tables %>%
  pluck("PatientInfo.csv") %>% 
  write_tsv(path = "data/patient_info_data.tsv")

# PatientRoute data
# ------------------------------------------------------------------------------
dataset_tables %>%
  pluck("PatientRoute.csv") %>% 
  write_tsv(path = "data/patient_route_data.tsv")

# Time data
# ------------------------------------------------------------------------------
dataset_tables %>%
  pluck("Time.csv") %>% 
  write_tsv(path = "data/time_data.tsv")

# TimeAge data
# ------------------------------------------------------------------------------
dataset_tables %>%
  pluck("TimeAge.csv") %>% 
  write_tsv(path = "data/time_age_data.tsv")

# TimeGender data
# ------------------------------------------------------------------------------
dataset_tables %>%
  pluck("TimeGender.csv") %>% 
  write_tsv(path = "data/time_gender_data.tsv")

# TimeProvince data
# ------------------------------------------------------------------------------
dataset_tables %>%
  pluck("TimeProvince.csv") %>% 
  write_tsv(path = "data/time_province_data.tsv")

# SearchTrend data
# ------------------------------------------------------------------------------
dataset_tables %>%
  pluck("SearchTrend.csv") %>% 
  write_tsv(path = "data/search_trend_data.tsv")

# Regional data
# ------------------------------------------------------------------------------
dataset_tables %>%
  pluck("Region.csv") %>% 
  write_tsv(path = "data/region_data.tsv")
