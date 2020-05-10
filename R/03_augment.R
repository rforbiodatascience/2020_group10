# Clear workspace ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries ------------------------------------------------------------------------------
library(tidyverse)

# Define functions ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Wrangle data ------------------------------------------------------------------------------

# Case augment
case_df <- read_tsv("data/case_data_clean.tsv")

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
  select(case_id, confirmed, case_type)

# Patient augment
patient_info_df <- read_tsv("data/patient_info_data_clean.tsv")
patient_route_df <- read_tsv("data/patient_route_data_clean.tsv")

# Joining the the two patient data frames
patient_df <- patient_info_df %>%
  full_join(patient_route_df,
    by = "patient_id",
    suffix = c("_patient_info", "_patient_route"))

patient_df <- patient_df %>%
  # Remove unnecessary columns
  select(-sex) %>%
  # add an age column and one more column to subset the ages into age_group
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

# Time augment
time_df           <- read_tsv("data/time_data_clean.tsv")
time_age_df       <- read_tsv("data/time_age_data_clean.tsv")
time_gender_df    <- read_tsv("data/time_gender_data_clean.tsv")
time_province_df  <- read_tsv("data/time_province_data_clean.tsv")
search_trend_df   <- read_tsv("data/search_trend_data_clean.tsv")

# Full join time series data by date. Suffix are added for col collisions
time_df <- time_df %>%
  full_join(time_age_df,
    by = "date",
    suffix = c("", "_time_age")
  ) %>%
  full_join(time_gender_df,
    by = "date",
    suffix = c("", "_time_gender")
  ) %>%
  full_join(time_province_df,
    by = "date",
    suffix = c("", "_time_province")
  ) %>%
  full_join(search_trend_df, by = "date")

# Removing unwanted columns (time*4)
time_df <- time_df %>%
  select(-time, -time_time_age, -time_time_gender, -time_time_province)

# Region augment
city_df <- read_tsv("data/city_data_clean.tsv")

city <- city_df %>% 
  select(-code, -latitude, -longitude, -province)

# TODO
# -	Add joining of patient data and city_df together.
# -	Update PCA with new city_df

# Write data to file --------------------------------------------

# Write to disk
write_tsv(case_df, "data/case_data_augmented.tsv")

# Write to disk
write_tsv(patient_df, "data/patient_data_augmented.tsv")

# Write to disk
write_tsv(time_df, "data/time_data_augmented.tsv")

# Write to disk
write_tsv(city_df, "data/city_data_augmented.tsv")


