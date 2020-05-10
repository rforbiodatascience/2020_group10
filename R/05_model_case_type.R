# Clear workspace -------------------------------------------------------------
rm(list = ls())

# Load libraries --------------------------------------------------------------
library(tidyverse)

# Define functions-------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data ---------------------------------------------------------------
case_df <- read_tsv("data/case_data_augmented.tsv")


# Wrangle data ----------------------------------------------------------------
case_df <- case_df %>%
  select(case_id, confirmed, case_type) %>%
  distinct()

case_sum <- case_df %>%
  group_by(case_type) %>%
  summarise(sum(confirmed)) %>%
  rename(sum = "sum(confirmed)") %>%
  arrange(sum)

# Visualization ---------------------------------------------------------------

case_plot <- case_sum %>%
  ggplot(mapping = aes(
    x = reorder(case_type, sum),
    y = sum,
    fill = case_type
  )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sum), vjust = -0.5) +
  ylim(0, 5800) +
  theme(
    axis.text.x = element_text(size = 10, angle = 65, vjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    title = "Infected people per case type",
    x = "",
    y = "Infected people"
  )

# Write plots and data to file ------------------------------------------------

ggsave(
  filename = "results/case_plot.png",
  plot = case_plot,
  width = 10,
  height = 5
)

