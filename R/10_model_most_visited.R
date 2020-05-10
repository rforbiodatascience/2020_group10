# Clear workspace------------------------------------------------------------------------------
rm(list = ls())

# Load libraries------------------------------------------------------------------------------
library(tidyverse)

# Define functions------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data------------------------------------------------------------------------------
patient_df <- read_tsv("data/patient_data_augmented.tsv")

# Filter the data------------------------------------------------------------------------------
patient_df <- patient_df %>% 
  group_by(type) %>% 
  drop_na()

# Visualize the data------------------------------------------------------------------------------

mvp_plot <- ggplot(
  patient_df,
  aes (x= type)
  ) +
  geom_bar(
    fill= heat.colors(5)
    ) +
  coord_flip() +
  labs(
    title = "Most visited places", 
       y = "Number of patients", 
       x = "Place", 
       caption ="Data from Korea Centers for Disease Control & Prevention (2020)"
    ) 

# Save the plot------------------------------------------------------------------------------
ggsave(
  filename = "results/10_most_visited_places.png", 
  width = 8, 
  height = 8, 
  mvp_plot)

  