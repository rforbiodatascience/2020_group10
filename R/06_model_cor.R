# Clear workspace ---------------------------------------------------------------
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)

# Define functions ---------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
city_df <- read_tsv("data/city_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

# Select columns of interest for the cor heatmap
city_df <- city_df %>%
  select(elementary_school_count:nursing_home_count)

# Get correlation matrix and round numbers
city_cor_matrix <- city_df %>%
  cor() %>%
  round(2)

# Keep only the lower triangle of matrix
city_cor_matrix <- city_cor_matrix %>%
  get_lower_tri()

# Pivot the matrix into dataframe for plotting
city_cor_df <- city_cor_matrix %>%
  as.data.frame() %>%
  mutate(var1 = factor(row.names(.), levels=row.names(.))) %>% 
  gather(key = var2, value = value, -var1, na.rm = TRUE, factor_key = TRUE) 

# Visualise data ----------------------------------------------------------
correlation_heatmap <- city_cor_df %>%
  ggplot(
    aes(x = var1, y = var2, fill = value)
  ) +
  geom_tile(
    color = "white",
  ) +
  scale_fill_gradient2(
    low = "#44344F",
    high = "#98A6D4",
    mid = "#56638A",
    na.value = "white",
    midpoint = 0,
    limit = c(-1, 1),
    name = "Pearson Correlation"
  ) +
  theme_minimal() +
  theme_group10 +
  geom_text(
    aes(var1, var2, label = value),
    color = "white",
    size = 4
  ) +
  theme(
    axis.text.x = element_text(angle = 45, size = 10, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16),
  ) +
  labs(
    title = "Regional city variable correlation heatmap\nof South Korea",
    x = "",
    y = "",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  )

# Write plots and data to file --------------------------------------------
ggsave(
  filename = "results/06_cor_heatmap.png",
  plot = correlation_heatmap,
  width = 10,
  height = 8
)

write_tsv(city_cor_df, "data/wrangled_city_var_cor.tsv")
