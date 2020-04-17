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
case_df <- case_df %>% 
  mutate(city = replace (city, city == "-", "other")) %>% 
  mutate(city = replace (city, city == "from other city", "other")) %>% 
  mutate(infection_case = replace(infection_case, infection_case == "etc", "other")) %>% 
  mutate(latitude = replace(latitude, latitude == "-", NA)) %>% 
  mutate(longitude = replace(longitude, longitude == "-", NA))

# patient_df cleaning
# ------------------------------------------------------------------------------
#Delete unwanted columns
patient_df$disease <- NULL
patient_df$contact_number <- NULL
patient_df$global_num_patient_route <- NULL
#we can use select instead of NULL

patient_df <- patient_df %>% 
  mutate(infection_case = replace_na(infection_case, "other")) %>% 
  mutate(infection_case = replace(infection_case, infection_case == "etc", "other")) %>% 
  mutate(state = replace_na(state, "unspecified")) %>% 
  mutate(sex = case_when(sex == "male" ~ 0,
                         sex == "female" ~ 1)) %>% 
#mutate(sex = replace_na(sex, "unspecified")) 
  mutate(country = replace_na(country, "other")) %>% 
  mutate(type = replace_na(type, "other")) %>%
  mutate(type = replace(type, type == "etc", "other")) %>% 
  mutate(province_patient_route = replace(province_patient_route, province_patient_route == "etc", "unspecified")) %>% 
  mutate(year_new = 2020 - birth_year) 
  

#### mutate(year_new = 2020 - birth_year)  %>% 
# mutate(year_new = case_when( year_new < 10 ~ "0s",
#                            year_new <= 10 ~ "10s", 
#                             year_new <= 20 ~ "20s",
#year_new <= 30 ~ "30s",
#year_new <= 40 ~ "40s",
#year_new <= 50 ~ "50s",
#year_new <= 60 ~ "50s",
#year_new <= 70 ~ "70s",
#year_new <= 80 ~ "80s",
#year_new <= 90 ~ "90s",
#year_new >= 100 ~ "100s"))

# time_df cleaning
# ------------------------------------------------------------------------------

# Adding two gender columns "male" and "female"
time_df <- time_df %>% 
  mutate(female = case_when(sex == "female" ~1,sex == "male" ~ 0)) %>% 
  mutate(male = case_when(sex == "male" ~1, sex == "female" ~ 0))
                                                 

#Removing unwanted columns (time*4 and sex)
time_df <- time_df %>%  select(-time, -time_time_age, -time_time_gender, -time_time_province, -sex)
