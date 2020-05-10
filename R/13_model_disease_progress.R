# Clear workspace ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries ------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)

# Define functions ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data ------------------------------------------------------------------------------
time_df <- read_tsv("data/time_data_augmented.tsv")

# Wrangle the data ------------------------------------------------------------------------------
time_df <- time_df %>%
  filter(date > ymd(20200120))

# Overall disease progress
disease_progress <- time_df %>%
  select(c(date, sex, test, negative, confirmed)) %>%
  pivot_wider(values_fn = list(confirmed = summary_fun), names_from = sex) %>%
  pivot_longer(c(test, confirmed, negative), names_to = "key", values_to = "value")

# Stratification by sex
confirmed_gender <- time_df %>%
  select(c(date, sex, confirmed_time_gender)) %>%
  group_by(date, confirmed_time_gender, sex) %>%
  summarise() %>%
  drop_na()

deceased_gender <- time_df %>%
  select(c(date, sex, deceased_time_gender)) %>%
  group_by(date, deceased_time_gender, sex) %>%
  summarise() %>%
  drop_na()

# Stratification by age
disease_age <- time_df %>%
  select(c(date, age, confirmed_time_age)) %>%
  group_by(date, confirmed_time_age, age) %>%
  summarise() %>%
  drop_na()


# Data visualization ------------------------------------------------------------------------------

# Disease progress
disease_progress_plot <- disease_progress %>%
  ggplot(
    aes(x = date, y = value, colour = key)
  ) +
  geom_line(
    na.rm = TRUE,
    size = 0.7
  ) +
  labs(
    x = "Time",
    y = "Tests",
    title = "Tested people for COVID-19 in 2020",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  ) +
  scale_x_date(
    date_labels = "%b %d",
    date_breaks = "week"
  ) +
  scale_color_discrete(
    name = "",
    labels = c("confirmed", "negtive", "total tested")
  )


# Stratified by gender
confirmed_gender_plot <- confirmed_gender %>%
  ggplot(
    aes(x = date, y = confirmed_time_gender, colour = sex)
  ) +
  geom_line(
    na.rm = TRUE,
    size = 0.7
  ) +
  labs(
    x = "Time",
    y = "Tests",
    title = "Confirmed tests of COVID-19 in 2020 - by gender",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  ) +
  scale_x_date(
    date_labels = "%b %d",
    date_breaks = "week"
  )

deceased_gender_plot <- deceased_gender %>%
  ggplot(
    aes(x = date, y = deceased_time_gender, colour = sex)
  ) +
  geom_line(
    na.rm = TRUE,
    size = 0.7
  ) +
  labs(
    x = "Time",
    y = "Deceased",
    title = "COVID-19 deaths in 2020 - by gender",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  ) +
  scale_x_date(
    date_labels = "%b %d",
    date_breaks = "week"
  )


# Stratified by age
disease_age_plot <- disease_age %>%
  ggplot(
    aes(x = date, y = confirmed_time_age, color = age)
  ) +
  geom_line(
    na.rm = TRUE
  ) +
  labs(
    x = "Time",
    y = "Tests",
    title = "Confirmed tests of COVID-19 in 2020 - by age",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)",
    color = "Age Group"
  ) +
  scale_x_date(
    date_labels = "%b %d",
    date_breaks = "week"
  ) +
  theme(legend.position = "bottom")


# Save the plots ------------------------------------------------------------------------------
ggsave(
  filename = "results/13_disease_progress.png",
  plot = disease_progress_plot
)
ggsave(
  filename = "results/13_confirmed_gender.png",
  plot = confirmed_gender_plot
)
ggsave(
  filename = "results/13_deceased_gender.png",
  plot = deceased_gender_plot
)
ggsave(
  filename = "results/13_disease_age.png",
  plot = disease_age_plot
)

# Save the data frame ------------------------------------------------------------------------------
write_tsv(
  x = disease_progress,
  path = "data/wrangled_disease_progres.tsv"
)
write_tsv(
  x = deceased_gender,
  path = "data/disease_gender.tsv"
)
write_tsv(
  x = disease_age,
  path = "data/disease_age_plot.tsv"
)
