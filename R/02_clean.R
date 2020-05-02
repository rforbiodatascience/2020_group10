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

# patient_info_df cleaning
# ------------------------------------------------------------------------------
# Increase guess_max to read all columns the right type
patient_info_df <- read_tsv("data/patient_info_data.tsv", guess_max = 3000)

#Delete unwanted columns patient_info_df
patient_info_df <- patient_info_df %>%
  select(-c(disease, contact_number, age, infection_order))

patient_info_df <- patient_info_df %>%
  mutate(infection_case = replace_na(infection_case, "other")) %>%
  mutate(infection_case = replace(infection_case, infection_case == "etc", "other")) %>%
  mutate(state = replace_na(state, "unspecified")) %>%
  mutate(country = replace_na(country, "other")) %>%
  mutate(province = replace(province, province == "etc", "other")) %>%
  mutate(city = replace(city, city == "etc", "unspecified")) %>%
  mutate(city = replace_na(city, "unspecified"))

write_tsv(patient_info_df, "data/patient_info_data_clean.tsv")

# patient_route_df cleaning
# ------------------------------------------------------------------------------
# Increase guess_max to read all columns the right type
patient_route_df <- read_tsv("data/patient_route_data.tsv")

#Delete unwanted columns patient_route_df
patient_route_df <- patient_route_df %>%
  select(-global_num)

patient_route_df <- patient_route_df %>%
  mutate(type = replace_na(type, "other")) %>%
  mutate(type = replace(type, type == "etc", "other")) %>%
  mutate(province = replace(province, province == "etc", "unspecified")) %>%
  mutate(province = replace_na(province, "unspecified")) %>%
  mutate(city = replace_na(city, "unspecified"))

write_tsv(patient_route_df, "data/patient_route_data_clean.tsv")

# time_df cleaning
# ------------------------------------------------------------------------------
time_df <- read_tsv("data/time_data.tsv")
write_tsv(time_df, "data/time_data_clean.tsv")

#time_age_df cleaning
#------------------------------------------------------------------------------
time_age_df <- read_tsv("data/time_age_data.tsv")
write_tsv(time_age_df, "data/time_age_data_clean.tsv")

#time_gender_df cleaning
#------------------------------------------------------------------------------
time_gender_df <- read_tsv("data/time_gender_data.tsv")
write_tsv(time_gender_df, "data/time_gender_data_clean.tsv")

#time_province_df cleaning
#------------------------------------------------------------------------------
time_province_df <- read_tsv("data/time_province_data.tsv")
write_tsv(time_province_df, "data/time_province_data_clean.tsv")

#search_trend_df cleaning
#------------------------------------------------------------------------------
search_trend_df <- read_tsv("data/search_trend_data.tsv")
write_tsv(search_trend_df, "data/search_trend_data_clean.tsv")



# region_data cleaning
# ------------------------------------------------------------------------------
region_df <- read_tsv("data/region_data.tsv")

# Filter cities and small regions
#(add prefix to city for small regions with no explicitly stated cities)
city_df <- region_df %>%
  filter(province != city | city == "Jeju-do" | city == "Sejong") %>%
  mutate(city = case_when(
    city == province ~ str_c("unknown_", city),
    TRUE ~ city
  ))

# Filter for provinces
province_df <- region_df %>%
  filter(province == city & province != "Korea")

# Get country summary for control calculations
country_df <- region_df %>%
  filter(province == "Korea")

# Write clean region dataframes to disk
write_tsv(province_df, "data/province_data_clean.tsv")
write_tsv(city_df, "data/city_data_clean.tsv")
write_tsv(country_df, "data/country_data_clean.tsv")
