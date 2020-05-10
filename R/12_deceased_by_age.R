# Clear workspace ---------------------------------------------------------------
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)

# Define functions ---------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
time_df <- read_tsv("data/time_data_augmented.tsv")

# Wrangle data ---------------------------------------------------------------

# Stratefy confirmed and deceased by age
time_age_disease <- time_df %>%
  select(confirmed_time_age, deceased_time_age, age) %>% 
  drop_na() %>% 
  group_by(age) %>% 
  nest()

# Apply linear model to the nested table
time_age_disease <- time_age_disease %>% 
  mutate(model = map(data, deceased_model))

# Get model stats and unnest. Filter unce
time_age_disease <- time_age_disease %>%
  mutate(result = map(model, broom::tidy, conf.int = TRUE)) %>% 
  unnest(result)

# Keep only slope estimate and create estimate percentage column
time_age_disease <- time_age_disease %>% 
  filter(term != "(Intercept)") %>%
  mutate(estimate_percentage = str_c(round(estimate * 100, 2), "%"))

# Visualization ---------------------------------------------------------------
  
deceased_by_confirmed_plot <- time_age_disease %>%
  ggplot(mapping = aes(x = estimate, y = age, label = estimate_percentage)) +
  geom_point() +
  theme_bw() +
  geom_label(nudge_y = 0.4) +
  geom_errorbarh(aes(xmax = conf.high, xmin = conf.low, height=.2)) +
  theme(plot.title = element_text(size = 18)) +
  labs(
    title = "Estimated deceased foreach confirmed case by age",
    subtitle = "Labels show estimate in percentage",
    x = "estimate",
    y = "age",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  )
  
# Write plots and data to file --------------------------------------------
  
ggsave(
  filename = "results/08_age_deceased_ratio.png",
  plot = deceased_by_confirmed_plot,
  width = 8,
  height = 8,
)
  
  