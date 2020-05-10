# Clear workspace --------------------------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------------------------
library(tidyverse)

# Define functions --------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Read the data -----------------------------------------------------------------------------
case_df <- read_tsv("data/case_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------------------------

# Get sum of confirmed by case_type
case_sum <- case_df %>%
  distinct() %>% 
  group_by(case_type) %>%
  summarise(sum = sum(confirmed)) %>%
  arrange(sum)

# Visualize data ------------------------------------------------------------------------------

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
    y = "Infected people",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  )

# Write plot and data ------------------------------------------------------------------------------

ggsave(filename = "results/05_model_case_type.png", 
       width = 10,
       height = 5,
       case_plot)

write_tsv(case_sum, "data/wrangled_case_type_sum.tsv")

