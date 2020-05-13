# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggrepel)

# Load functions ----------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
time_df <- read_tsv(file = "data/time_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

# Extract relevant data
new_cases_coronavirus <- time_df %>%
  select(date, coronavirus, confirmed) %>%
  filter(date >= as.Date("2020-01-01")) %>%
  rename(coronavirus_search_trend = coronavirus) %>%
  distinct()

# Create column with new cases from accumulated cases
new_cases_coronavirus <- new_cases_coronavirus %>%
  mutate(
    confirmed_cases_per_day = confirmed - lag(confirmed),
    confirmed_cases_per_day = replace_na(confirmed_cases_per_day, 0)
  ) %>%
  select(-confirmed) %>%
  pivot_longer(.,
    cols = c(confirmed_cases_per_day, coronavirus_search_trend),
    names_to = "parameter",
    values_to = "value"
  )

# Visualise data ----------------------------------------------------------

# Prepare new facet label names, dates and annotations
plot_labels <- as_labeller(c(
  "confirmed_cases_per_day" =
  "Number of new confirmed COVID-19 cases per day",
  "coronavirus_search_trend" =
  "Search activity for 'coronavirus' relative to maximum"
))

dates <- c(
  "2020-01-03",
  "2020-01-20",
  "2020-02-07",
  "2020-02-18",
  "2020-03-07",
  "2020-03-28"
)

annotation_upper <- tibble(
  x = as.Date(dates),
  label = c(
    "Wuhan travellers\nquarantined",
    "National alert\nlevel increased",
    "Test capacity\nboost",
    "Church super-\nspreader confirmed",
    "GPS-enforced\nquarantine",
    "Recovered >\nisolated"
  ),
  parameter = "confirmed_cases_per_day"
)

vertical_lines <- tibble(x = as.Date(dates), parameter = "coronavirus_search_trend")

# Plot the data with annotations
trend_plot <- new_cases_coronavirus %>%
  ggplot(aes(date)) +
  geom_segment(annotation_upper, mapping = aes(x = x, y = 0, xend = x, yend = 775), colour = "grey") +
  geom_vline(vertical_lines, mapping = aes(xintercept = x), colour = "grey") +
  geom_text_repel(data = annotation_upper, mapping = aes(x = x, y = 1000, label = label), size = 2.5) +
  geom_line(aes(y = value, colour = parameter)) +
  facet_wrap(~parameter, scales = "free_y", nrow = 2, labeller = plot_labels) +
  labs(
    title = "COVID-19 cases and coronavirus search trend",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  ) +
  xlab("Time (2020)") +
  ylab("") +
  theme_group10 +
  theme(
    legend.position = "none",
    plot.subtitle = element_text(size = 9)
  ) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b")

# Write plots and data to file --------------------------------------------
ggsave(
  filename = "results/15_searchtrend.png",
  plot = trend_plot,
  width = 10,
  height = 5
)

write_tsv(x = new_cases_coronavirus, path = "data/wrangled_cases_searchtrend.tsv")

# Detach external packages ---------------------------------------------------------------------
detach("package:ggrepel", unload=TRUE)
