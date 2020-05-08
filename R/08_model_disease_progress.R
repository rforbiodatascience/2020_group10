# Clear workspace ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(lubridate)

# Define functions ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data ------------------------------------------------------------------------------
time_df <- read_tsv("data/time_data_augmented.tsv")
time_gender_df <- read_tsv("data/time_gender_data_clean.tsv")
time_age_df <- read_tsv("data/time_age_data_clean.tsv")

# Wrangle the data ------------------------------------------------------------------------------

#maybe we can leave the sex as female and male instead of binary in the augmenting
disease_progress <- time_df %>% 
  select(c(date, sex, test, negative, confirmed)) %>% 
  mutate(sex = case_when(sex == 1 ~ "female",
                         sex == 0 ~ "male")) %>% 
  filter(date > ymd(20200120))  %>%
  pivot_wider(values_fn = list(confirmed = summary_fun),  names_from= sex) %>% 
  pivot_longer(c(`test`, `confirmed`, `negative`), names_to = "key", values_to = "value")

# Stratification by sex
disease_gender <- time_df %>% 
  select(c(date, sex, confirmed_time_gender)) %>% 
  mutate(sex = case_when(sex == 1 ~ "female",
                         sex == 0 ~ "male")) %>% 
  filter(date > ymd(20200120))  %>%
  group_by(date, confirmed_time_gender, sex) %>% 
  summarise() %>% 
  drop_na() 

# Stratification by age
disease_age <- time_df %>% 
  select(c(date, age, confirmed_time_age)) %>% 
  filter(date > ymd(20200120))  %>%
  group_by(date, confirmed_time_age, age) %>% 
  summarise() %>% 
  drop_na()


# Data visualization ------------------------------------------------------------------------------

# Disease progress
disease_progress_plot <- disease_progress %>% 
  ggplot(
    aes(x= date, y= value, colour=key)
    ) +
  geom_line(
    na.rm = TRUE, 
            size= 0.7
            ) +
  labs(x = "Month", 
       y = "Tests", 
       title= "Tested people for COVID-19 in 2020", 
       caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
       )+
  scale_x_date(
    date_labels = "%B", 
    date_breaks = "month"
    ) + 
  scale_color_discrete(name = "",
                       labels = c("confirmed", "negtive", "total tested")
                       )


# Stratified by gender
disease_gender_plot <- disease_gender %>% 
  ggplot(
    aes(x = date, y = confirmed_time_gender, colour = sex)
    ) +
  geom_line(
    na.rm = TRUE, 
    size= 0.7
    ) +
  labs(
    x = "Month", 
    y = "Tests", 
    title = "Confirmed tests of COVID-19 in 2020 - by gender",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
    ) +
  scale_x_date(
    date_labels = "%B", 
    date_breaks = "month"
    ) 


#Stratified by age
disease_age_plot <- disease_age %>% 
  ggplot(
    aes(x= date, y= confirmed_time_age, color= age)
    ) +
  geom_line(
    na.rm = TRUE
    ) +
  labs(x = "Month", 
       y = "Tests", 
       title= "Confirmed tests of COVID-19 in 2020 - by age", 
       caption = "Data from Korea Centers for Disease Control & Prevention (2020)",
       color= "Age Group"
       ) +
  scale_x_date(
    date_labels = "%B",
    date_breaks = "month"
    ) +
  theme(legend.position = "bottom")
  

# Save the plots ------------------------------------------------------------------------------
ggsave(
  filename = "results/disease_progress_plot.png", 
  plot = disease_progress_plot
  )
ggsave(
  filename = "results/disease_gender_plot.png", 
  plot = disease_gender_plot
  )
ggsave(
  filename = "results/disease_age_plot.png",
  plot = disease_age_plot
  )

# Save the data frame ------------------------------------------------------------------------------
write_tsv(
  x = disease_progress, 
  path = "data/wrangled_disease_progres.tsv"
  )
write_tsv(
  x = disease_gender, 
  path = "data/disease_gender.tsv"
  )
write_tsv(
  x = disease_age, 
  path = "data/disease_age_plot.tsv"
  )

