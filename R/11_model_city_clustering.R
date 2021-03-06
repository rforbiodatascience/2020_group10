# Clear workspace ---------------------------------------------------------------
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(patchwork)

# Define functions ---------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
city_conf_pca_aug <- read_tsv("data/wrangled_city_pca.tsv")
city_conf_pca_explained <- read_tsv("data/wrangled_city_pca_explained.tsv")

# Wrangle data ------------------------------------------------------------
set.seed(5)

center_clusters <- 4

# K-means clustering of original data
city_conf_pca_orig <- city_conf_pca_aug %>%
  select(elementary_school_count:nursing_home_count) %>%
  kmeans(centers = center_clusters)

city_conf_kmean_orig_aug <- city_conf_pca_orig %>%
  broom::augment(city_conf_pca_aug) %>%
  rename(cluster_org = .cluster)

# K-means clustering of PC1 and PC2
city_conf_kmean_pca <- city_conf_pca_aug %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = center_clusters)

city_conf_kmean_pca_aug <- city_conf_kmean_pca %>%
  broom::augment(city_conf_kmean_orig_aug) %>%
  rename(cluster_pca = .cluster)

var_explained_vector <- city_conf_pca_explained %>%
  pluck("percent_round") %>% 
  as.vector()

# Visualise data ----------------------------------------------------------

# Plot K-mean clustering on original data
pl1 <- city_conf_kmean_pca_aug %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_org)) +
  geom_point() +
  theme_group10 +
  theme(
    legend.position = c(.95, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2),
    legend.box.background = element_rect(colour = "black"),
    plot.title = element_text(size = 18)
  ) +
  scale_color_manual(
    values = c("lightgreen", "yellow", "red", "darkred")
  ) +
  labs(
    title = "K-means clustering (k=4) \noriginal variables",
    x = str_c("PC1 (", var_explained_vector[1], "%)"),
    y = str_c("PC2 (", var_explained_vector[2], "%)"),
    color = "Clusters"
  )

# Plot K-mean clustering on PC1 and PC2
pl2 <- city_conf_kmean_pca_aug %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme_group10 +
  theme(
    legend.position = c(.95, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(2, 2, 2, 2),
    legend.box.background = element_rect(colour = "black"),
    plot.title = element_text(size = 18)
  ) +
  scale_color_manual(
    values = c("lightgreen", "yellow", "red", "darkred")
  ) +
  labs(
    title = "K-means clustering (k=4) \nPC1 and PC2 ",
    x = str_c("PC1 (", var_explained_vector[1], "%)"),
    y = str_c("PC2 (", var_explained_vector[2], "%)"),
    color = "Clusters"
  )

# Combine all plots using patchwork
combined_plots <- (pl1 + pl2) +
  plot_annotation(
    title = "Clustering comparison of regional city data",
    subtitle = "Unsupervised prediction of confirmed cases by k-means clustering",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  ) +
  plot_annotation(
    theme = theme(
      plot.title = element_text(size = 29),
      plot.caption = element_text(size = 11),
      plot.subtitle = element_text(size = 22)
    )
  )

# Predict clustering accuracy
clustering_pred <- city_conf_kmean_pca_aug %>%
  select(class_label, cluster_org, cluster_pca) %>%
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
      class_label == cluster_org ~ 1,
      class_label != cluster_org ~ 0
    ),
    cluster_pca_correct = case_when(
      class_label == cluster_pca ~ 1,
      class_label != cluster_pca ~ 0
    )
  ) %>%
  summarise(
    score_org = mean(cluster_org_correct),
    score_pca = mean(cluster_pca_correct)
  )

# Write plots and data to file --------------------------------------------
ggsave(
  filename = "results/11_city_clustering.png",
  plot = combined_plots,
  width = 10,
  height = 10,
)

write_tsv(clustering_pred, "results/wrangled_cluster_pred.tsv")

# Detach external packages ---------------------------------------------------------------------
detach("package:patchwork", unload=TRUE)
