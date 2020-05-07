# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(lubridate)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data
# ------------------------------------------------------------------------------
time_df <- read_tsv("data//time_data_augmented.tsv")
time_gender_df <- read_tsv("data//time_gender_data_clean.tsv")
time_age_df <- read_tsv("data//time_age_data_clean.tsv")

# Wrangle the data
# ------------------------------------------------------------------------------
disease_progress <- time_df %>% 
  select(c(date, sex, test, negative, confirmed)) %>% 
  mutate(sex = case_when(sex == 1 ~ "female",
                         sex == 0 ~ "male")) %>% 
  filter(date > ymd(20200120))  %>%
  pivot_wider(values_fn = list(confirmed = summary_fun),  names_from= sex) %>% 
  pivot_longer(c(`test`, `confirmed`, `negative`), names_to = "key", values_to = "value")

#disease_progress <- time_df %>% 
#  select(c(date, age, sex, test, negative, confirmed)) %>% 
#  mutate(sex = case_when(sex == 1 ~ "female",
#                         sex == 0 ~ "male")) %>%
#  filter(date > ymd(20200120))  %>%
#  pivot_longer(c(`test`, `confirmed`, `negative`), names_to = "key", values_to = "value") 

# Stratification by sex
timegender <- time_gender_df %>% 
  select(-time)

# Stratification by age
time_age <- time_age_df %>% 
  select(-time) %>% 
  group_by(date, age)

# Data visualization
# ------------------------------------------------------------------------------
disease_progress_plot <- disease_progress %>% 
  ggplot(aes(x= date, y= value, colour=key)) +
  geom_line(na.rm = TRUE, size= 0.7) +
  labs(x = "Month", 
       y = "Tests", 
       title= "Tested people for COVID-19 in 2020") +
  scale_x_date(date_labels = "%B", 
               date_breaks = "month") + 
  scale_color_discrete(name="",
                       labels = c("confirmed", "negtive", "total tested"))

timegender_plot <- timegender %>% 
  ggplot(aes(x= date, y= confirmed, colour= sex)) +
  geom_line(na.rm = TRUE, size= 0.7) +
  labs(x = "Month", y = "Confirmed tests", title= "Confirmed tests of COVID-19 in 2020") +
  scale_x_date(date_labels = "%B", date_breaks = "month") 

timegender_age <- time_age %>% 
  ggplot(aes(x= date, y= confirmed, color= age)) +
  geom_line(na.rm = TRUE) +
  labs(x = "Month", y = "Confirmed tests", title= "Confirmed tests of COVID-19 in 2020") +
  scale_x_date(date_labels = "%B", date_breaks = "month") 


# Save the plots 
# ------------------------------------------------------------------------------
ggsave(filename = "results/disease_progress_plot.png", plot = disease_progress_plot)
ggsave(filename = "results/time_gender_plot.png", plot = timegender_plot)
ggsave(filename = "results/time_gender_age.png", plot = timegender_age)

# Save the data frame 
# ------------------------------------------------------------------------------
write_tsv(x = disease_progress, path = "data/wrangled_disease_progres.tsv")
write_tsv(x = timegender, path = "data/wrangled_time_gender.tsv")

