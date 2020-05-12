# Clear workspace ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries ------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(broom)

# Define functions ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data ------------------------------------------------------------------------------
time_df <- read_tsv("data/time_data_augmented.tsv")

# Wrangle the data ------------------------------------------------------------------------------
time_df <- time_df %>%
  filter(date > ymd(20200120))

# Overall disease progress
confirmed_progress <- time_df %>% 
  select(c(date, sex, test, negative, confirmed, deceased)) %>% 
  pivot_wider(values_fn = list(confirmed = summary_fun),  names_from= sex) %>% 
  pivot_longer(c(test, confirmed, negative, deceased), names_to = "key", values_to = "value")

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
confirmed_age <- time_df %>%
  select(c(date, age, confirmed_time_age)) %>%
  group_by(date, confirmed_time_age, age) %>%
  summarise() %>%
  drop_na()

# Logistic modelling
# Extract relevant data
time_df_to_model <- confirmed_progress %>% 
  filter(key == "confirmed" | key == "deceased") %>%
  mutate(date = date - as.Date("2020-01-01")) %>%
  mutate(date = as.integer(date)) %>% 
  distinct() 

# Do regression to find asymptote and peak day
model_summary <- time_df_to_model %>%
  group_by(key) %>%
  nest() %>%
  mutate(model = map(data, sigm_model)) %>% 
  mutate(tidied = map(model, broom::tidy)) %>% 
  unnest(tidied) 

features_to_plot <- model_summary %>% 
  filter(term == "Asym" | term == "xmid") %>% 
  select(-c(statistic, model, p.value, std.error)) %>% 
  pivot_wider(., names_from = term, values_from = estimate)

# Calculate deceased and confirmed as estimated by model
fitted_data <- time_df_to_model %>% 
  group_by(key) %>% 
  do(fit = sigm_model(.)) %>%
  broom::augment(fit)


# Data visualization ------------------------------------------------------------------------------

# Disease progress
confirmed_progress_plot <- confirmed_progress %>%
  filter(key != "deceased") %>% 
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
    labels = c("confirmed", "negative", "total tested")
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
confirmed_age_plot <- confirmed_age %>%
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

# Logistic modelling
logistic_plot <- fitted_data %>% 
  ggplot() +
  geom_point(aes(x= date, y= value, colour=key)
  ) +
  geom_line(aes(x= date, y= .fitted), colour = "black", alpha = 0.5
  ) +
  facet_wrap(~key, nrow=4, scales = "free_y") +
  geom_point(data = features_to_plot, colour = "black", aes(x = xmid, y = Asym/2), shape = 19) + 
  geom_hline(data = features_to_plot, aes(yintercept=Asym), colour = "black", alpha = 0.5, linetype = "dashed", size = 0.8) +
  labs(
    x = "Days after January 1st, 2020", 
    y = "Number of people", 
    title = "Logistic modelling of the course of the epidemic",
    subtitle = "Number of confirmed and deceased people over time fitted to a logistic model",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  ) +
  theme(legend.position = "none")

# Save the plots ------------------------------------------------------------------------------
ggsave(
  filename = "results/13_disease_progress.png",
  plot = confirmed_progress_plot,
  height = 8,
  width = 10
)
ggsave(
  filename = "results/13_confirmed_gender.png",
  plot = confirmed_gender_plot,
  height = 8,
  width = 10
)
ggsave(
  filename = "results/13_deceased_gender.png",
  plot = deceased_gender_plot,
  height = 8,
  width = 10
)
ggsave(
  filename = "results/13_disease_age.png",
  plot = confirmed_age_plot,
  height = 8,
  width = 10
)
ggsave(
  filename = "results/13_logistic_model.png",
  plot = logistic_plot,
  height = 8,
  width = 10
)

# Save the data frame ------------------------------------------------------------------------------
write_tsv(
  x = confirmed_progress,
  path = "data/wrangled_disease_progres.tsv"
)
write_tsv(
  x = deceased_gender,
  path = "data/disease_gender.tsv"
)
write_tsv(
  x = confirmed_age,
  path = "data/disease_age_plot.tsv"
)

# remove features that cannot be written in a tidy way
model_summary <- model_summary %>%
  select(-c(data, model))

write_tsv(
  x = model_summary,
  path = "results/logistic_model.tsv"
)
