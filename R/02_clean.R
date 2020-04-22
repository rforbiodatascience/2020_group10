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
patient_df <- patient_df %>% select(., -c(disease, contact_number, global_num_patient_route, age, infection_order))

patient_df <- patient_df %>% 
  mutate(infection_case = replace_na(infection_case, "other")) %>% 
  mutate(infection_case = replace(infection_case, infection_case == "etc", "other")) %>% 
  mutate(state = replace_na(state, "unspecified")) %>% 
  mutate(country = replace_na(country, "other")) %>% 
  mutate(type = replace_na(type, "other")) %>%
  mutate(type = replace(type, type == "etc", "other")) %>% 
  mutate(province_patient_route = replace(province_patient_route, province_patient_route == "etc", "unspecified")) %>%
  mutate(province_patient_route = replace_na(province_patient_route, "unspecified")) %>% 
  mutate(province_patient_info = replace(province_patient_info, province_patient_info == "etc", "other")) %>% 
  mutate(city_patient_info = replace(city_patient_info, city_patient_info == "etc", "unspecified")) %>% 
  mutate(city_patient_info = replace_na(city_patient_info, "unspecified")) %>% 
  mutate(city_patient_route = replace_na(city_patient_route, "unspecified"))
  

# Write clean patient dataframe to disk
write_tsv(patient_df, "data/patient_data_clean.tsv")

# time_df cleaning
# ------------------------------------------------------------------------------
time_df <- read_tsv("data/time_data.tsv")


# Write clean time dataframe to disk
write_tsv(time_df, "data/time_data_clean.tsv")

# region_data cleaning
# ------------------------------------------------------------------------------
region_df <- read_tsv("data/region_data.tsv")

# Filter cities and small regions (add prefix to city for small regions with no explicitly stated cities)
city_df <- region_df %>% 
  filter(province != city | city == "Jeju-do" |city == "Sejong") %>% 
  mutate(city = case_when(city == province ~ paste0("unknown_", city),
                          TRUE ~ city))
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
