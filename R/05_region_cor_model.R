# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(reshape2)

# Define functions
# ------------------------------------------------------------------------------
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

# Keep only lower triangle of matrix

city_cor_matrix <- city_cor_matrix %>% 
  get_lower_tri()

# Pivot the matrix into dataframe for plotting

city_cor_df <- city_cor_matrix %>%
  melt()

# Visualise data ----------------------------------------------------------

correlation_heatmap <- city_cor_df %>% 
  ggplot(
    aes(x = Var1, y = Var2, fill = value)
  ) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#132B43",
    high = "#132B43",
    na.value = "white",
    midpoint = 0,
    limit = c(-1, 1),
    space = "Lab",
    name = "Pearson Correlation"
  ) +
  geom_text(
    aes(Var1, Var2, label = value), color = "white", size = 4
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 24),
  ) +
  ggtitle("Korean Covid19 Regional Correlational Heatmap") +
  xlab("") +
  ylab("")

# Write plots and data to file --------------------------------------------
ggsave(
  filename = "results/05_correlation_heatmap.png",
  plot = correlation_heatmap
)
