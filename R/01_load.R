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
case_df <- dataset_tables$Case.csv

# Write case data disk
write_tsv(case_df, path = "data/case_data.tsv")

# Patient data
# ------------------------------------------------------------------------------

# Full join patient data by patient_id. Suffix are added for col collisions
patient_df <- dataset_tables$PatientInfo.csv %>%
  full_join(dataset_tables$PatientRoute.csv, by = "patient_id", 
            suffix = c("_patient_info", "_patient_route"))

# Write patient data to disk
write_tsv(patient_df, path = "data/patient_data.tsv")

# Time series data
# ------------------------------------------------------------------------------

# Full join time series data by date. Suffix are added for col collisions
time_df <- dataset_tables$Time.csv %>%
  full_join(dataset_tables$TimeAge.csv, by = "date",
            suffix = c("", "_time_age")) %>%
  full_join(dataset_tables$TimeGender.csv, by = "date",
            suffix = c("", "_time_gender")) %>%
  full_join(dataset_tables$TimeProvince.csv, by = "date",
            suffix = c("", "_time_province"))
  full_join(dataset_tables$SearchTrend.csv, by = "date")
  
# Write time data to disk
write_tsv(time_df, path = "data/time_data.tsv")

# Regional data
# ------------------------------------------------------------------------------
region_df <- dataset_tables$Region.csv

# Write region data to disk 
write_tsv(region_df, path = "data/region_data.tsv")