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

#  Join all Time* sets without column duplicates
time_df <- dataset_korean_covid19$Time.csv %>%
  full_join(dataset_korean_covid19$TimeAge.csv, by = "date") %>%
  full_join(dataset_korean_covid19$TimeGender.csv, by = "date") %>%
  full_join(dataset_korean_covid19$TimeProvince.csv, by = "date") %>%
  full_join(dataset_korean_covid19$SearchTrend.csv, by = "date") %>%
  select(-contains("."))

# Write dataframe
write_tsv(x = time_df, path = "data/01_dat_load.tsv")

# Patient dataframe
# ------------------------------------------------------------------------------

# Join Case, PatientInfo and PatientRoute sets without column duplicates
patient_df <- dataset_korean_covid19$Case.csv %>%
  full_join(dataset_korean_covid19$PatientInfo.csv, by = "infection_case") %>%
  full_join(dataset_korean_covid19$PatientRoute.csv, by = "patient_id") %>%
  select(-contains("."))

# Write dataframe
write_tsv(x = patient_df, path = "data/02_dat_load.tsv")

# Region dataframe
# ------------------------------------------------------------------------------

# Join Region and Weather sets
region_df <- dataset_korean_covid19$Region.csv %>%
  full_join(dataset_korean_covid19$Weather.csv, by = "province")

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