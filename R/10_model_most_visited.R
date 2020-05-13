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
  drop_na() %>% 
  summarise(count = n()) %>%
  arrange(count)

# Visualize the data------------------------------------------------------------------------------

mvp_plot <- patient_df %>%
  ggplot(aes(x = reorder(type, count), y = count, fill = type)) +
  geom_bar(stat = "identity") +
  theme_group10 +
  theme(
    axis.text.x = element_text(size = 14, angle = 90, hjust=0.95, vjust=0.25),
    legend.position = "none"
  ) +
  labs(
    title = "Most visited places",
    y = "Number of patients",
    x = "Place",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  )

# Save the plot------------------------------------------------------------------------------
ggsave(
  filename = "results/10_most_visited_places.png",
  width = 8,
  height = 8,
  mvp_plot
)