# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggrepel)
library(patchwork)

# library(devtools)
#install_github("thomasp85/patchwork")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
city_df <- read_tsv("data/city_data_augmented.tsv")
patient_df <- read_tsv("data/patient_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

set.seed(5)

# K-means clustering of original data

center_clusters <- 4

city_conf_pca_orig <- city_conf_pca_aug %>%
  select(elementary_school_count:nursing_home_count) %>%
  kmeans(centers = center_clusters)

city_conf_kmean_orig_aug <- city_conf_pca_orig %>%
  broom::augment(city_conf_pca_aug) %>%
  rename(cluster_org = .cluster)

city_conf_kmean_pca <- city_conf_pca_aug %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = center_clusters)

city_conf_kmean_pca_aug <- city_conf_kmean_pca %>%
  broom::augment(city_conf_kmean_orig_aug) %>%
  rename(cluster_pca = .cluster)

# Visualise data ----------------------------------------------------------

# Plot original PCA

pl1 <- city_conf_kmean_pca_aug %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = class)) +
  geom_point() +
  theme(
    legend.position = c(.95, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2),
    legend.box.background = element_rect(colour = "black")
  ) +
  scale_color_manual(
    values = c("lightgreen", "yellow", "red", "darkred")
  ) +
  labs(
    title = "Original PCA",
    x = "PC1",
    y = "PC2",
    color = "Confirmed cases"
  )

# Plot K-mean clustering on original data
pl2 <- city_conf_kmean_pca_aug %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_org)) +
  geom_point() +
  theme(
    legend.position = c(.95, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2),
    legend.box.background = element_rect(colour = "black")
  ) +
  scale_color_manual(
    values = c("lightgreen", "yellow", "red", "darkred")
  ) +
  labs(
    title = "K-means clustering using\noriginal variables (k=4)",
    x = "PC1",
    y = "PC2",
    color = "Clusters"
  )

# Plot K-mean clustering on PC1 and PC2
pl3 <- city_conf_kmean_pca_aug %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(
    legend.position = c(.95, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2),
    legend.box.background = element_rect(colour = "black")
  ) +
  scale_color_manual(
    values = c("lightgreen", "yellow", "red", "darkred")
  ) +
  labs(
    title = "K-means clustering using\nPC1 and PC2 (k=4)",
    x = "PC1",
    y = "PC2",
    color = "Clusters"
  )

# Combine all plots using patchwork
combined_plots <- (pl1 + pl2 + pl3) +
  plot_annotation(
    title = "Clustering comparison of regional city data from South Korea",
    subtitle = "Unsupervised prediction of confirmed cases by applying k-means clustering ",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  )

# Predict clustering accuracy
clustering_pred <- city_conf_kmean_pca_aug %>%
  select(class, cluster_org, cluster_pca) %>%
  mutate(
    cluster_org = case_when(
      cluster_org == 1 ~ "3. Moderate",
      cluster_org == 2 ~ "4. High",
      cluster_org == 3 ~ "2. Low",
      cluster_org == 4 ~ "1. None"
    ),
    cluster_pca = case_when(
      cluster_pca == 1 ~ "2. Low",
      cluster_pca == 2 ~ "4. High",
      cluster_pca == 3 ~ "3. Moderate",
      cluster_pca == 4 ~ "1. None"
    ),
    cluster_org_correct = case_when(
      class == cluster_org ~ 1,
      class != cluster_org ~ 0
    ),
    cluster_pca_correct = case_when(
      class == cluster_pca ~ 1,
      class != cluster_pca ~ 0
    )
  ) %>%
  summarise(
    score_org = mean(cluster_org_correct),
    score_pca = mean(cluster_pca_correct)
  )

# Write plots and data to file --------------------------------------------

ggsave(
  filename = "results/07_clustering.png",
  plot = combined_plots
)
