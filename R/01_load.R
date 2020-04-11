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

# Nest TimeAge, TimeProvince and TimeGender and append to Time
# ------------------------------------------------------------------------------
time_files_labels = c("TimeAge.csv", "TimeProvince.csv", "TimeGender.csv")

# Nest TimeAge, TimeProvince and TimeGender 
nested_time_files <- dataset_korean_covid19[time_file_labels] %>%
  map(group_by, date) %>%
  map(nest) %>% 
  reduce(full_join, by='date')

# Rename nested names
old_names <- colnames(test)[2:length(test)]

nested_time_files <- nested_time_files %>% 
  rename_at(vars(old_names), ~ str_replace(time_dataframes, ".csv", ""))

# Join nested TimeAge, TimeProvince and TimeGender to Time
test %>% full_join(dataset_korean_covid19$Time.csv, by='date') %>% 
  arrange(date)
