# Clear workspace ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries -------------------------------------------------------------------------------
library(tidyverse)

# Define functions -----------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data --------------------------------------------------------------------------------
case_df <- read_tsv("data/case_data.tsv")

# guess_max for correct type columns
patient_info_df <- read_tsv("data/patient_info_data.tsv", guess_max = 3000)
patient_route_df <- read_tsv("data/patient_route_data.tsv")

time_df <- read_tsv("data/time_data.tsv")
time_age_df <- read_tsv("data/time_age_data.tsv")
time_gender_df <- read_tsv("data/time_gender_data.tsv")
time_province_df <- read_tsv("data/time_province_data.tsv")

search_trend_df <- read_tsv("data/search_trend_data.tsv")

region_df <- read_tsv("data/region_data.tsv")

# Cleaning case dataframe  ---------------------------------------------------------------------

# Replace unknowns to same identity
case_df <- case_df %>%
  mutate(
    city = replace(city, city == "-", "other"),
    city = replace(city, city == "from other city", "other"),
    infection_case = replace(infection_case, infection_case == "etc", "other"),
    latitude = replace(latitude, latitude == "-", NA),
    longitude = replace(longitude, longitude == "-", NA)
  )

# Cleaning patient info -------------------------------------------------------------------------

# Delete unwanted columns patient_info_df
patient_info_df <- patient_info_df %>%
  select(-c(disease, contact_number, age, infection_order))

# Clean up inconsistent string values
patient_info_df <- patient_info_df %>%
  mutate(
    infection_case = replace_na(infection_case, "other"),
    infection_case = replace(infection_case, infection_case == "etc", "other"),
    state = replace_na(state, "unspecified"),
    country = replace_na(country, "other"),
    province = replace(province, province == "etc", "other"),
    city = replace(city, city == "etc", "unspecified"),
    city = replace_na(city, "unspecified")
  )

# Remove self infection and make new variable of infected and infecting person.
patient_info_df <- patient_info_df %>%
  mutate(infected_by = replace(infected_by, infected_by == patient_id, NA)) %>%
  rowwise() %>%
  mutate(inf_pair = str_c(sort(c(patient_id, infected_by)), collapse = ",")) %>%
  ungroup()

# Introduce NA where two people are eachothers infected_by
patient_info_df <- patient_info_df %>%
  group_by(inf_pair) %>%
  mutate(infected_by = case_when(
    n() == 1 ~ infected_by 
  )) %>%
  ungroup() %>%
  select(-inf_pair)

# Cleaning patient route dataframe --------------------------------------------------------------

# Delete unwanted columns patient_route_df
patient_route_df <- patient_route_df %>%
  select(-global_num)

# Clean up inconsistent string values
patient_route_df <- patient_route_df %>%
  mutate(
    type = replace_na(type, "other"),
    type = replace(type, type == "etc", "other"),
    province = replace(province, province == "etc", "unspecified"),
    province = replace_na(province, "unspecified"),
    city = replace_na(city, "unspecified")
  )

# Cleaning region dataframe ---------------------------------------------------------------------

# Summarize the amount of reoccuring provinces
city_df <- region_df %>%
  group_by(province) %>%
  mutate(count = length(province)) %>%
  ungroup()

# Remove rows with same cities as province, unless the province only exist once
city_df <- city_df %>%
  filter(province != city | count < 2) %>%
  filter(province != "Korea") %>% 
  select(-count)

# Add prefix to cities that are named same as province
city_df <- city_df %>% 
  mutate(city = case_when(
    city == province ~ str_c(city, "_city"),
    TRUE ~ city
  ))

# Write data to file ----------------------------------------------------------------------------

write_tsv(case_df, "data/case_data_clean.tsv")

write_tsv(time_df, "data/time_data_clean.tsv")
write_tsv(time_age_df, "data/time_age_data_clean.tsv")
write_tsv(time_gender_df, "data/time_gender_data_clean.tsv")
write_tsv(time_province_df, "data/time_province_data_clean.tsv")

write_tsv(patient_route_df, "data/patient_route_data_clean.tsv")
write_tsv(patient_info_df, "data/patient_info_data_clean.tsv")

write_tsv(search_trend_df, "data/search_trend_data_clean.tsv")

write_tsv(city_df, "data/city_data_clean.tsv")
