# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data
# ------------------------------------------------------------------------------
patient_df <- read_tsv("data/patient_data_augmented.tsv")

# Filter the data
# ------------------------------------------------------------------------------
patient_df <- patient_df %>% 
  group_by(type) %>% 
  filter(n()>100 & n()<3000)


# Visualize the data
# ------------------------------------------------------------------------------

mvp_plot <- ggplot(patient_df, mapping = aes (x= type)) +
  geom_bar(fill= heat.colors(7)) +
  coord_flip() +
  labs(title="MOST VISITED PLACES", y="Number of patients", x= "Place")

# Save the plot
# ------------------------------------------------------------------------------
ggsave(
  filename = "results/most_visited_places.png", mvp_plot)
  