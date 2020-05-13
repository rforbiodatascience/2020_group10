# Clear workspace ---------------------------------------------------------------
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggrepel) # Non-overlapping ggplot labels
library(patchwork) # Combining plots

# Define functions ---------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
city_df <- read_tsv("data/city_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

# Quantile of confirmed cases
q_conf <- city_df %>% 
  select(confirmed) %>% 
  map(quantile)

# Convert quantile to vector list
q_conf <- q_conf %>%
  as.data.frame(col.names = "values") %>%
  pluck("values")

# Classify confirmed cases into 4 quantile classes
city_df <- city_df %>%
  mutate(class_label = case_when(
    confirmed >= q_conf[1] & confirmed <= q_conf[2] ~ "1. None",
    confirmed >= q_conf[2] & confirmed <= q_conf[3] ~ "2. Low",
    confirmed >= q_conf[3] & confirmed <= q_conf[4] ~ "3. Moderate",
    confirmed >= q_conf[4] & confirmed <= q_conf[5] ~ "4. High",
    TRUE ~ "0"
  ))

# Apply PCA
city_conf_pca <- city_df %>%
  select(elementary_school_count:nursing_home_count) %>%
  prcomp(center = TRUE, scale. = TRUE)

# Augment the PCA with columns from the city dataframe
city_conf_pca_aug <- city_conf_pca %>%
  broom::augment(city_df)

# Get PCA eigenvectors
pca_vectors <- city_conf_pca %>%
  pluck("rotation") %>%
  as_tibble(rownames = NA) %>% 
  rownames_to_column()

city_conf_pca <- city_conf_pca %>% 
  broom::tidy("pcs") %>%
  mutate(percent_round = round(percent*100, 0))

# Create variance explained vector
var_explained_vector <- city_conf_pca %>%
  pluck("percent_round") %>% 
  as.vector()

# Visualise data ----------------------------------------------------------

# Plot the variance explained of each PC
pca_var <- city_conf_pca %>%
  ggplot(aes(x = PC, y = percent, label = str_c(round(percent * 100, 0), "%"))) +
  geom_col(fill = "#98A6D4") +
  geom_text(size = 6, position = position_stack(vjust = 0.5), fontface = "bold") +
  scale_x_continuous("PC", breaks = 1:10) +
  theme_minimal() +
  labs(
    title = "Explained variance of PCA\nfrom regional city data",
    x = "PC",
    y = "Variance explained (%)"
  ) +
  theme_group10 +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

# Plot PC1 and PC2 and their corresponding eigenvectors
pca_plot <- city_conf_pca_aug %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, color = class_label)) +
  geom_point() +
  scale_color_manual(
    values = c("lightgreen", "yellow", "red", "darkred")
  ) +
  geom_segment(
    data = pca_vectors,
    aes(x = 0, y = 0, xend = (PC1 * 5), yend = (PC2 * 5)),
    arrow = arrow(length = unit(0.1, "cm")),
    color = "grey"
  ) +
  geom_label_repel(
    data = pca_vectors,
    aes(x = PC1 * 5, y = PC2 * 5, label = rowname),
    size = 5,
    inherit.aes = FALSE
  ) +
  theme_group10 +
  labs(
    title = "PCA of regional city data",
    subtitle = "Classified by the quantile of confirmed cases from each city",
    color = "Confirmed cases",
    x = str_c("PC1 (", var_explained_vector[1], "%)"),
    y = str_c("PC2 (", var_explained_vector[2], "%)"),
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  )

# Write plots and data to file --------------------------------------------
ggsave(
  filename = "results/08_city_pca_variance.png",
  plot = pca_var,
  width = 8,
  height = 8,
)

ggsave(
  filename = "results/08_city_pca.png",
  plot = pca_plot,
  width = 10,
  height = 10,
)

write_tsv(city_conf_pca_aug, "data/wrangled_city_pca.tsv")
write_tsv(city_conf_pca, "data/wrangled_city_pca_explained.tsv")
write_tsv(pca_vectors, "data/wrangled_pca_vectors.tsv")

# Detach external packages ---------------------------------------------------------------------
detach("package:ggrepel", unload=TRUE)
detach("package:patchwork", unload=TRUE)


