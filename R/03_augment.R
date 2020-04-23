# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Case augment
# ------------------------------------------------------------------------------
case_df <- read_tsv("data/case_data_clean.tsv")

# Patient augment
# ------------------------------------------------------------------------------
patient_df <- read_tsv("data/patient_data_clean.tsv", guess_max = 3000)

patient_df <- patient_df %>%
  # transform gender to binary
  mutate(sex = case_when(
    sex == "male" ~ 0,
    sex == "female" ~ 1
  )) %>%
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
patient_df %>%
  unite("state_date", released_date : deceased_date, remove = TRUE) %>%
  mutate(state_date = str_replace_all(state_date, "[_NA]", ""))

# Write to disk
write_tsv(patient_df, "data/patient_data_augmented.tsv")

# Time augment
# ------------------------------------------------------------------------------
time_df <- read_tsv("data/time_data_clean.tsv")

# Adding a binary gender column
time_df <- time_df %>%
  mutate(sex = case_when(
    sex == "male" ~ 0,
    sex == "female" ~ 1
  ))

# Removing unwanted columns (time*4)
time_df <- time_df %>%
  select(-time, -time_time_age, -time_time_gender, -time_time_province)

# Write to disk
write_tsv(time_df, "data/time_data_augmented.tsv")

# Region augment
# ------------------------------------------------------------------------------
city_df <- read_tsv("data/city_data_clean.tsv")

# Use the suffixes to identify divisions in
city_df <- city_df %>%
  mutate(division = case_when(
    str_ends(city, "-do") ~ "province",
    str_ends(city, "-si") ~ "city",
    str_ends(city, "-gun") ~ "county",
    str_ends(city, "-gu") ~ "district",
    TRUE ~ "city"
  ))

# Write to disk
write_tsv(city_df, "data/city_data_augmented.tsv")
