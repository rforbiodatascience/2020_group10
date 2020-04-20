# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# case_df cleaning
# ------------------------------------------------------------------------------
case_df <- read_tsv("data/case_data.tsv")
case_df <- case_df %>%
  mutate(city = replace(city, city == "-", "other")) %>%
  mutate(city = replace(city, city == "from other city", "other")) %>%
  mutate(infection_case = replace(infection_case, infection_case == "etc", "other")) %>%
  mutate(latitude = replace(latitude, latitude == "-", NA)) %>%
  mutate(longitude = replace(longitude, longitude == "-", NA))

# Write clean case dataframe to disk
write_tsv(case_df, "data/case_data_clean.tsv")

# patient_df cleaning
# ------------------------------------------------------------------------------

# Increase guess_max to read all columns the right type

patient_df <- read_tsv("data/patient_data.tsv", guess_max = 3000)

#Delete unwanted columns
patient_df <- patient_df %>% select(., -c(disease, contact_number, global_num_patient_route, age ))

patient_df <- patient_df %>% 
  mutate(infection_case = replace_na(infection_case, "other")) %>% 
  mutate(infection_case = replace(infection_case, infection_case == "etc", "other")) %>% 
  mutate(state = replace_na(state, "unspecified")) %>% 
  mutate(sex = case_when(sex == "male" ~ 0,
                         sex == "female" ~ 1)) %>% 
  mutate(country = replace_na(country, "other")) %>% 
  mutate(type = replace_na(type, "other")) %>%
  mutate(type = replace(type, type == "etc", "other")) %>% 
  mutate(province_patient_route = replace(province_patient_route, province_patient_route == "etc", "unspecified")) %>% 
  mutate(age = 2020 - birth_year) %>%
  mutate(age_group = case_when( birth_year < 10 ~ "0s",
                          age < 20 ~ "10s", 
                          age < 30 ~ "20s",
                          age < 40 ~ "30s",
                          age < 50 ~ "40s", 
                          age < 60 ~ "50s",
                          age < 70 ~ "60s",
                          age < 80 ~ "70s",
                          age < 90 ~ "80s",
                          age < 100 ~ "90s",
                          age >= 100 ~ "100s")) %>%
  select(-birth_year)

# Write clean patient dataframe to disk
write_tsv(patient_df, "data/patient_data_clean.tsv")

# time_df cleaning
# ------------------------------------------------------------------------------
time_df <- read_tsv("data/time_data.tsv")
# Adding two gender columns "male" and "female"
time_df <- time_df %>% 
  mutate(sex = case_when(sex == "male" ~ 0,
                         sex == "female" ~ 1))
 
#Removing unwanted columns (time*4 and sex)
time_df <- time_df %>% select(-time, -time_time_age, -time_time_gender, -time_time_province)

# Write clean time dataframe to disk
write_tsv(time_df, "data/time_data_clean.tsv")

# region_data cleaning
# ------------------------------------------------------------------------------
region_df <- read_tsv("data/region_data.tsv")

# Write clean region dataframe to disk
write_tsv(region_df, "data/region_data_clean.tsv")
