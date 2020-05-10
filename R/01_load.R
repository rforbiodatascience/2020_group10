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
<<<<<<< HEAD
  pluck("Case.csv") %>%
=======
  pluck("Case.csv") %>% 
>>>>>>> f1800d851bb1b03bde48f8f1fe50917c6fb16322
  write_tsv(path = "data/case_data.tsv")

# PatientInfo data
# ------------------------------------------------------------------------------
dataset_tables %>%
<<<<<<< HEAD
  pluck("PatientInfo.csv") %>%
=======
  pluck("PatientInfo.csv") %>% 
>>>>>>> f1800d851bb1b03bde48f8f1fe50917c6fb16322
  write_tsv(path = "data/patient_info_data.tsv")

# PatientRoute data
# ------------------------------------------------------------------------------
dataset_tables %>%
<<<<<<< HEAD
  pluck("PatientRoute.csv") %>%
=======
  pluck("PatientRoute.csv") %>% 
>>>>>>> f1800d851bb1b03bde48f8f1fe50917c6fb16322
  write_tsv(path = "data/patient_route_data.tsv")

# Time data
# ------------------------------------------------------------------------------
dataset_tables %>%
<<<<<<< HEAD
  pluck("Time.csv") %>%
=======
  pluck("Time.csv") %>% 
>>>>>>> f1800d851bb1b03bde48f8f1fe50917c6fb16322
  write_tsv(path = "data/time_data.tsv")

# TimeAge data
# ------------------------------------------------------------------------------
dataset_tables %>%
<<<<<<< HEAD
  pluck("TimeAge.csv") %>%
=======
  pluck("TimeAge.csv") %>% 
>>>>>>> f1800d851bb1b03bde48f8f1fe50917c6fb16322
  write_tsv(path = "data/time_age_data.tsv")

# TimeGender data
# ------------------------------------------------------------------------------
dataset_tables %>%
<<<<<<< HEAD
  pluck("TimeGender.csv") %>%
=======
  pluck("TimeGender.csv") %>% 
>>>>>>> f1800d851bb1b03bde48f8f1fe50917c6fb16322
  write_tsv(path = "data/time_gender_data.tsv")

# TimeProvince data
# ------------------------------------------------------------------------------
dataset_tables %>%
<<<<<<< HEAD
  pluck("TimeProvince.csv") %>%
=======
  pluck("TimeProvince.csv") %>% 
>>>>>>> f1800d851bb1b03bde48f8f1fe50917c6fb16322
  write_tsv(path = "data/time_province_data.tsv")

# SearchTrend data
# ------------------------------------------------------------------------------
dataset_tables %>%
<<<<<<< HEAD
  pluck("SearchTrend.csv") %>%
=======
  pluck("SearchTrend.csv") %>% 
>>>>>>> f1800d851bb1b03bde48f8f1fe50917c6fb16322
  write_tsv(path = "data/search_trend_data.tsv")

# Regional data
# ------------------------------------------------------------------------------
dataset_tables %>%
<<<<<<< HEAD
  pluck("Region.csv") %>%
=======
  pluck("Region.csv") %>% 
>>>>>>> f1800d851bb1b03bde48f8f1fe50917c6fb16322
  write_tsv(path = "data/region_data.tsv")
