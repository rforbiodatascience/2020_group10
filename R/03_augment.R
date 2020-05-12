# Clear workspace -------------------------------------------------------------------------------
rm(list = ls())

# Load libraries --------------------------------------------------------------------------------
library(tidyverse)

# Define functions ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data ---------------------------------------------------------------------------------
case_df <- read_tsv("data/case_data_clean.tsv")

patient_info_df <- read_tsv("data/patient_info_data_clean.tsv")
patient_route_df <- read_tsv("data/patient_route_data_clean.tsv")

time_df <- read_tsv("data/time_data_clean.tsv")
time_age_df <- read_tsv("data/time_age_data_clean.tsv")
time_gender_df <- read_tsv("data/time_gender_data_clean.tsv")
time_province_df <- read_tsv("data/time_province_data_clean.tsv")
search_trend_df <- read_tsv("data/search_trend_data_clean.tsv")

city_df <- read_tsv("data/city_data_clean.tsv")

# Augment case dataframe ------------------------------------------------------------------------

case_df <- case_df %>%
  mutate(case_type = case_when(
    str_detect(infection_case, "Hospital") ~ "hospital",
    str_detect(infection_case, "Church") ~ "church",
    str_detect(infection_case, "overseas") ~ "overseas",
    str_detect(infection_case, "Pilgrimage") ~ "overseas",
    str_detect(infection_case, "patient") ~ "contact with patient",
    str_detect(infection_case, "Call") ~ "call center",
    str_detect(infection_case, "Nursing") ~ "nursing home",
    str_detect(infection_case, "gym") ~ "gym",
    str_detect(infection_case, "Lab") ~ "lab",
    str_detect(infection_case, "Community Center") ~ "community center",
    str_detect(infection_case, "other") ~ "other",
    )) %>%
  mutate(case_type = replace(case_type, is.na(case_type), "other"))

# Remove unnecessary columns
case_df <- case_df %>%
  select(confirmed, case_type)

# Augment patient dataframes --------------------------------------------------------------------

# Joining the the two patient data frames
patient_df <- patient_info_df %>%
  full_join(patient_route_df, by = "patient_id", suffix = c("_patient_info", "_patient_route"))

# Remove unnecessary columns
patient_df <- patient_df %>%
  select(-sex) %>%
  
# Add age column and one more column to subset the ages into age_group
patient_df <- patient_df %>%
  mutate(age = 2020 - birth_year) %>%
  mutate(age_group = case_when(
    age < 10 ~ "0s",
    age < 20 ~ "10s",
    age < 30 ~ "20s",
    age < 40 ~ "30s",
    age < 50 ~ "40s",
    age < 60 ~ "50s",
    age < 70 ~ "60s",
    age < 80 ~ "70s",
    age < 90 ~ "80s",
    age < 100 ~ "90s",
    age >= 100 ~ "100s"
  )) %>%
  select(-birth_year)
  
# Combine 'released_date' and 'deceased_date' and use state column
# Unite merges NAs as str and have to be converted back to correct type
patient_df <- patient_df %>%
  unite("state_date", released_date : deceased_date, remove = TRUE) %>%
  mutate(state_date = str_replace_all(state_date, "[_NA]", "")) %>%
  mutate(state_date = ifelse(state_date %in% "", NA, state_date))

patient_df <- patient_df %>%
  select(-c(global_num, country, symptom_onset_date, age))

# Augment time dataframes -----------------------------------------------------------------------

# Full join time series data by date. Suffix are added for col collisions
time_df <- time_df %>%
  full_join(time_age_df, by = "date", suffix = c("", "_time_age")) %>%
  full_join(time_gender_df, by = "date", suffix = c("", "_time_gender")) %>%
  full_join(time_province_df, by = "date", suffix = c("", "_time_province")) %>%
  full_join(search_trend_df, by = "date")

# Removing unwanted columns
time_df <- time_df %>%
  select(-c(
    time,
    time_time_age,
    time_time_gender,
    time_time_province,
    released,
    province,
    confirmed_time_province,
    released_time_province,
    deceased_time_province,
    cold,
    flu,
    pneumonia
  ))

# Augment region dataframe ----------------------------------------------------------------------

# Removing unwanted columns
city_df <- city_df %>% 
  select(-c(code, latitude, longitude, province))

# Get city confirmed cases using patient dataframe
confirmed_cases <- patient_df %>%
  group_by(city_patient_info) %>%
  distinct(patient_id, .keep_all = TRUE) %>%
  rename("city" = city_patient_info) %>%
  summarise(confirmed = length(confirmed_date))

# Join city confirmed cases to city regional data
city_df <- confirmed_cases %>%
  full_join(city_df, by = "city") %>%
  mutate(confirmed = replace_na(confirmed, 0)) %>%
  drop_na()

# Write data to file ----------------------------------------------------------------------------

write_tsv(case_df, "data/case_data_augmented.tsv")
write_tsv(patient_df, "data/patient_data_augmented.tsv")
write_tsv(time_df, "data/time_data_augmented.tsv")
write_tsv(city_df, "data/city_data_augmented.tsv")