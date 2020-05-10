# Clear Workspace ---------------------------------------------------------
rm(list = ls())
options(scipen=999)

# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load functions ----------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
patient_df <- read_tsv(file = "data/patient_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

# select relevant part of data and make group of 'contact with patient' and 'other
infection_case_df <- patient_df %>% 
  filter(state == "released" | state == "isolated") %>%
  select(patient_id, state, state_date, confirmed_date, age_group, infection_case) %>% 
  mutate(infection_case = case_when(
    infection_case == "contact with patient" ~ infection_case,
    TRUE ~ "other"))

# calculate duration of time in isolation or time from confirmed to released
infection_case_df <- infection_case_df %>%
  mutate(duration = case_when(
    state == "released" ~ state_date - confirmed_date,
    state == "isolated" ~ as.Date("2020-04-06") - confirmed_date)) %>% 
  select(-state_date) %>% 
  drop_na() %>% 
  distinct()

# group observations and summarize
infection_case_df <- infection_case_df %>%
  group_by(infection_case, state, age_group) %>% 
  summarise(count = n(),
            sd =sd(duration),
            average_duration = as.numeric(mean(duration))) %>% 
  mutate(age_group = factor(age_group, levels = c("0s", "10s", "20s", "30s", "40s", "50s", "60s", "70s", "80s", "90s", "100s")))  

# Visualise data ----------------------------------------------------------
# plot the data
exposure_plot <- ggplot(infection_case_df, aes(x=infection_case, y=average_duration, fill=age_group)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin=average_duration-sd, ymax=average_duration+sd), width=.9, size = 0.4,
                position=position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(~ state, nrow = 2)

# add labels
exposure_plot <- exposure_plot +
  labs(
    title ="Duration of disease for COVID-19 patients in South Korea", 
    subtitle = "Days in from isolation since confimation for isolatated patients (panel 1) and time from confirmed to released (panel 2). \nStratified on age group and infection case.",  
    color = "Age group",
    caption ="Data from Korea Centers for Disease Control & Prevention (2020)") +
  xlab("Infection case") +
  ylab("Average duration (days)") 

# Write plots and data to file --------------------------------------------
ggsave(filename = "results/exposure_plot.png", plot = exposure_plot, height = 8, width = 10)
write_tsv(x = infection_case_df, path = "data/wrangled_exposure.tsv")
