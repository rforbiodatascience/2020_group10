# Clear Workspace ---------------------------------------------------------
rm(list = ls())
options(scipen = 999)

# Load libraries ----------------------------------------------------------
library(tidyverse)

# Load functions ----------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
patient_df <- read_tsv(file = "data/patient_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

# Select relevant part of data and make group of 'contact with patient' and 'other
infection_case_df <- patient_df %>%
  filter(state == "released" | state == "isolated") %>%
  select(patient_id, state, state_date, confirmed_date, age_group, infection_case) %>%
  mutate(infection_case = case_when(
    infection_case == "contact with patient" ~ infection_case,
    TRUE ~ "other"
  ))

# Calculate duration of time in isolation or time from confirmed to released
infection_case_df <- infection_case_df %>%
  mutate(duration = case_when(
    state == "released" ~ state_date - confirmed_date,
    state == "isolated" ~ as.Date("2020-04-06") - confirmed_date
  )) %>%
  select(-state_date) %>%
  drop_na() %>%
  distinct()

age_labels <- c("0s", "10s", "20s", "30s", "40s", "50s", "60s", "70s", "80s", "90s", "100s")

# Group observations and summarize
infection_case_df <- infection_case_df %>%
  group_by(infection_case, state, age_group) %>%
  summarise(
    count = n(),
    sd = sd(duration),
    average_duration = as.numeric(mean(duration))
  ) %>%
  mutate(age_group = factor(age_group, levels = age_labels))

# Visualise data ----------------------------------------------------------

# Create labels
label_names <- c(
  "isolated" = "Days in isolation since confimation for isolatated patients",
  "released" = "Days in isolation for released patients"
)


# Plot the data
exposure_plot <- infection_case_df %>% 
  ggplot(aes(x = infection_case, y = average_duration, fill = age_group)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_errorbar(aes(ymin = average_duration - sd, ymax = average_duration + sd),
    width = .9, size = 0.4,
    position = position_dodge2(width = 0.9, preserve = "single")
  ) +
  facet_wrap(~state, nrow = 2, labeller = as_labeller(label_names)) +
  theme_group10 +
  theme(strip.text.x = element_text(size = 8),
        plot.title = element_text(size = 23)) +
  labs(
    title = "Duration of disease for COVID-19 patients",
    color = "Age group",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  ) +
  xlab("Infection case") +
  ylab("Average duration (days)")

# Write plots and data to file --------------------------------------------¨

ggsave(
  filename = "results/14_contact_exposure.png",
  plot = exposure_plot,
  height = 6,
  width = 8
)

write_tsv(x = infection_case_df, path = "data/wrangled_exposure.tsv")
