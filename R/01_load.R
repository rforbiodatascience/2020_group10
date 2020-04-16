# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load all dataset files
# ------------------------------------------------------------------------------
dataset_raw_files <- list.files(path = "_raw", full.names = TRUE)

dataset_korean_covid19 <- dataset_raw_files %>%
  setNames(nm = sapply(str_split(., "/"), tail, 1)) %>%
  map(read_csv)

# Create Time dataframe
# ------------------------------------------------------------------------------

# Add suffixes to TimeAge and TimeGender to prevent column name collisions
dataset_korean_covid19$TimeAge.csv <- dataset_korean_covid19$TimeAge.csv %>% 
  rename_at(vars(-one_of('date')), ~ paste0(., '_age'))

dataset_korean_covid19$TimeGender.csv  <- dataset_korean_covid19$TimeGender.csv %>%
  rename_at(vars(-one_of('date')), ~ paste0(., '_gender'))

#  Join all Time* sets
time_df <- dataset_korean_covid19$Time.csv %>%
  full_join(dataset_korean_covid19$TimeAge.csv, by = "date") %>%
  full_join(dataset_korean_covid19$TimeGender.csv, by = "date") %>%
  full_join(dataset_korean_covid19$SearchTrend.csv, by = "date")

# Write dataframe
write_tsv(x = time_df, path = "data/01_dat_load.tsv")

# Patient dataframe
# ------------------------------------------------------------------------------

# Add suffixes to PatientInfo and PatientRoute to prevent column name collisions
dataset_korean_covid19$PatientInfo.csv <- dataset_korean_covid19$PatientInfo.csv %>% 
  rename_at(vars(-one_of('infection_case', 'patient_id', 'global_num')), ~ paste0(., '_patient_info'))

dataset_korean_covid19$PatientRoute.csv  <- dataset_korean_covid19$PatientRoute.csv %>%
  rename_at(vars(-one_of('patient_id', 'global_num')), ~ paste0(., '_patient_route'))

# Join Case, PatientInfo and PatientRoute sets
patient_df <- dataset_korean_covid19$PatientRoute.csv %>%
  full_join(dataset_korean_covid19$PatientInfo.csv, by = c("patient_id", "global_num"))

View(patient_df)

# Write dataframe
write_tsv(x = patient_df, path = "data/02_dat_load.tsv")

# Region dataframe
# ------------------------------------------------------------------------------

# Join Region and Weather sets without column duplicates
region_df <- dataset_korean_covid19$Region.csv %>%
  full_join(dataset_korean_covid19$Weather.csv, by = "province") %>% 
  full_join(dataset_korean_covid19$TimeProvince.csv, by = c("province", "date"))

View(region_df)

# Write dataframe 
write_tsv(x = region_df, path = "data/03_dat_load.tsv")

################################################################################

# **REMOVE IN FUTURE**
# ------------------------------------------------------------------------------

# Create Time dataframe (using nested lists)
# ------------------------------------------------------------------------------
#time_files_labels = c("TimeAge.csv", "TimeProvince.csv", "TimeGender.csv")
#
# Nest TimeAge, TimeProvince and TimeGender 
#nested_time_files <- dataset_korean_covid19[time_files_labels] %>%
#  map(group_by, date) %>%
#  map(nest) %>% 
#  reduce(full_join, by='date')
#
# Rename name of nested tibbles
#new_names < c("time_age", "time_province", "time_gender")
#old_names <- colnames(nested_time_files)[2:length(nested_time_files)]
#
#nested_time_files <- nested_time_files %>% 
#  rename_at(vars(old_names), ~ new_names)
#
# Join nested TimeAge, TimeProvince and TimeGender to Time
#time_df <- dataset_korean_covid19$Time.csv %>% 
#  full_join(nested_time_files, by='date') %>% 
#  arrange(date)
#
#View(time_df)
#
# Write dataframe
#write_tsv(x = time_df, path = "data/01_dat_load.tsv")