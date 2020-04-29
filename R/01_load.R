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
write_tsv(dataset_tables$Case.csv, path = "data/case_data.tsv")

# PatientInfo data
# ------------------------------------------------------------------------------
write_tsv(dataset_tables$PatientInfo.csv, path = "data/patient_info_data.tsv")

# PatientRoute data
# ------------------------------------------------------------------------------
write_tsv(dataset_tables$PatientRoute.csv, path = "data/patient_route_data.tsv")

# Time data
# ------------------------------------------------------------------------------
write_tsv(dataset_tables$Time.csv, path = "data/time_data.tsv")

# TimeAge data
# ------------------------------------------------------------------------------
write_tsv(dataset_tables$TimeAge.csv, path = "data/time_age_data.tsv")

# TimeGender data
# ------------------------------------------------------------------------------
write_tsv(dataset_tables$TimeGender.csv, path = "data/time_gender_data.tsv")

# TimeProvince data
# ------------------------------------------------------------------------------
write_tsv(dataset_tables$TimeProvince.csv, path = "data/time_province_data.tsv")

# SearchTrend data
# ------------------------------------------------------------------------------
write_tsv(dataset_tables$SearchTrend.csv, path = "data/search_trend_data.tsv")

# Regional data
# ------------------------------------------------------------------------------
write_tsv(dataset_tables$Region.csv, path = "data/region_data.tsv")
