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

# Region augment
# ------------------------------------------------------------------------------
region_df <- read_tsv("data/region_data_clean.tsv")

region_df <-region_df %>%
  mutate(division = case_when(str_ends(city, "-do") ~ "province",
                              str_ends(city, "-si") ~ "city",
                              str_ends(city, "-gun") ~ "county",
                              str_ends(city, "-gu") ~ "district",
                              TRUE ~ "city"))

province_summary <- region_df %>%
  filter(province == city) %>%
  group_by(province) %>%
  summarise_at(vars(contains("count")), sum, na.rm = FALSE)

city_summary <- region_df %>% 
  filter(province != city) %>%
  group_by(province) %>%
  summarise_at(vars(contains("count")), sum, na.rm = FALSE)

full_join(province_summary, city_summary, by = "province")


