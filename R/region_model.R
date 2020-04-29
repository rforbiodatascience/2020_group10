# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
province_df <- read_tsv("data/province_data_clean.tsv")
time_province_df <- read_tsv("data/time_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

# Get infection numbers from time dataframe of each province
time_province_df <- time_province_df %>%
  group_by(province) %>%
  summarise(
    confirmed = max(confirmed_time_province),
    released = max(confirmed_time_province),
    deceased = max(deceased_time_province)
  )

# Join province and infection numbers
province_df <- province_df %>%
  left_join(time_province_df, by = "province")

# Pivot longer province dataframe columns for correlation plot
vars_remove <- c("code", "city", "latitude", "longitude", "released", "deceased")

deceased_cor <- province_df %>%
  select(-vars_remove) %>% 
  pivot_longer(c(-confirmed, -province),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(confirmed > 0) %>% 
  arrange(desc(confirmed))

# Visualise data ----------------------------------------------------------

# Plot multiple correlation plots.
# Diseased against province society stats
correlation_plot <- ggplot(deceased_cor) +
  geom_jitter(aes(value, confirmed, colour = variable), ) +
  geom_smooth(aes(value, confirmed, colour = variable), method = lm) +
  facet_wrap(~variable, scales = "free") +
  labs(x = "Var value(s)", y = "N Deceased")

# Write plots and data to file --------------------------------------------
ggsave(
  filename = "results/correlation_province_confirmed.png",
  plot = correlation_plot
)
