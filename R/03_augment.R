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

# Time augment
# ------------------------------------------------------------------------------
time_df <- read_tsv("data/time_data_clean.tsv")

# Adding a binary gender column
time_df <- time_df %>% 
  mutate(sex = case_when(sex == "male" ~ 0,
                         sex == "female" ~ 1))

#Removing unwanted columns (time*4)
time_df <- time_df %>% select(-time, -time_time_age, -time_time_gender, -time_time_province)

# Write to disk
write_tsv(time_df, "data/time_data_augmented.tsv")

# Region augment
# ------------------------------------------------------------------------------
city_df <- read_tsv("data/city_data_clean.tsv")

# Use the suffixes to identify divisions in
city_df <-city_df %>%
  mutate(division = case_when(str_ends(city, "-do") ~ "province",
                              str_ends(city, "-si") ~ "city",
                              str_ends(city, "-gun") ~ "county",
                              str_ends(city, "-gu") ~ "district",
                              TRUE ~ "city"))
# Write to disk
write_tsv(city_df, "data/city_data_augmented.tsv")

