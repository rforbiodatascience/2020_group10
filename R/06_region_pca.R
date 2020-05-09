# Clear workspace ---------------------------------------------------------------
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggrepel) # Non-overlapping ggplot labels
library(patchwork) # Combining plots

# library(devtools)
#install_github("thomasp85/patchwork")

# Define functions ---------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
city_df <- read_tsv("data/city_data_augmented.tsv")
patient_df <- read_tsv("data/patient_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

# Get city confirmed cases using patient dataframe
confirmed_cases <- patient_df %>%
  group_by(city_patient_info) %>%
  distinct(patient_id, .keep_all = TRUE) %>%
  rename("city" = city_patient_info) %>%
  summarise(confirmed = length(confirmed_date))

# Join city confirmed cases to city regional data
city_conf_df <- confirmed_cases %>%
  full_join(city_df, by = "city") %>%
  mutate(confirmed = replace_na(confirmed, 0)) %>%
  drop_na()

# Quantile of confirmed cases
q_conf <- city_conf_df %>%
  select(confirmed) %>%
  map(quantile)

# Convert quantile to vector list
q_conf <- q_conf %>%
  as.data.frame(col.names = "values") %>%
  pluck("values")

# Classify confirmed cases into 4 quantile classes
city_conf_df <- city_conf_df %>%
  mutate(class = case_when(
    confirmed >= q_conf[1] & confirmed <= q_conf[2] ~ "1. None",
    confirmed >= q_conf[2] & confirmed <= q_conf[3] ~ "2. Low",
    confirmed >= q_conf[3] & confirmed <= q_conf[4] ~ "3. Moderate",
    confirmed >= q_conf[4] & confirmed <= q_conf[5] ~ "4. High",
    TRUE ~ "0"
  ))

# Apply PCA
city_conf_pca <- city_conf_df %>%
  select(elementary_school_count:nursing_home_count) %>%
  prcomp(center = TRUE, scale. = TRUE)

# Augment the PCA with columns from before PCA
city_conf_pca_aug <- city_conf_pca %>%
  broom::augment(city_conf_df)

# Get PCA eigenvectors
pca_vectors <- city_conf_pca %>%
  pluck("rotation") %>%
  data.frame(variables = rownames(.), .)

# Visualise data ----------------------------------------------------------

# Plot the variance explained of each PC
pca_var <- city_conf_pca %>%
  broom::tidy("pcs") %>%
  ggplot(aes(x = PC, y = percent, label = str_c(round(percent * 100, 0), "%"))) +
  geom_col(fill = "#98A6D4") +
  geom_text(size = 6, position = position_stack(vjust = 0.5), fontface = "bold") +
  scale_x_continuous("PC", breaks = 1:10) +
  theme_minimal() +
  labs(
    title = "Explained variance of PCA\nfrom regional city data in South Korea",
    x = "PC",
    y = "Percentage variance explained (%)"
  ) +
  theme(
    plot.title = element_text(size = 18),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

# Plot PC1 and PC2 and their corresponding eigenvectors
pca_plot <- city_conf_pca_aug %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, color = class)) +
  geom_point() +
  scale_color_manual(
    values = c("lightgreen", "yellow", "red", "darkred")
  ) +
  geom_segment(
    data = pca_vectors,
    aes(x = 0, y = 0, xend = (PC1 * 5), yend = (PC2 * 5)),
    arrow = arrow(length = unit(1 / 2, "picas")),
    color = "grey"
  ) +
  geom_label_repel(
    data = pca_vectors,
    aes(x = PC1 * 5, y = PC2 * 5, label = variables),
    size = 2,
    inherit.aes = FALSE
  ) +
  theme(plot.title = element_text(size = 18)) +
  labs(
    title = "PCA of regional city data in South Korea",
    subtitle = "Classified by the quantile of confirmed cases from each city",
    color = "Confirmed cases",
    x = "PC1",
    y = "PC2",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  )

# Write plots and data to file --------------------------------------------
ggsave(
  filename = "results/06_pca_variance.png",
  plot = pca_var,
  width = 8,
  height = 8,
)

ggsave(
  filename = "results/06_pca_plot.png",
  plot = pca_plot,
  width = 8,
  height = 8,
)

write_tsv(city_conf_pca_aug, "data/06_city_conf_pca_aug.tsv")

