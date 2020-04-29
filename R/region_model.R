# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Move to 
# ------------------------------------------------------------------------------

city_df <- read_tsv("data/city_data_augmented.tsv")

province_df <- city_df %>%
  group_by(province) %>% 
  summarise_at(vars(elementary_school_count:nursing_home_count), sum)

# Get province confirmed and non confirmed
time_province_df <- read_tsv("data/time_data_augmented.tsv")

time_province_df <- time_province_df %>% 
  group_by(province) %>%
  summarise(confirmed = max(confirmed_time_province),
            released = max(confirmed_time_province),
            deceased = max(deceased_time_province))

province_df <- province_df %>%
  left_join(time_province_df, by = "province")

############ CORRELATION PLOTS ########################

deceased_cor <- province_df %>% 
  pivot_longer(c(-deceased, -province), 
               names_to = "variable", 
               values_to = "value") %>% 
  filter(deceased > 0) %>% 
  arrange(desc(deceased))

ggplot(deceased_cor) +
  geom_jitter(aes(value, deceased, colour=variable),) + 
  geom_smooth(aes(value, deceased, colour=variable), method=lm) +
  facet_wrap(~variable, scales="free") +
  labs(x = "Values", y = "Death count")

############################ PCA ########################

# PCA of regional data
city_pca <- city_df %>%
  select_at(vars(elementary_school_count:nursing_home_count)) %>% 
  prcomp(center = TRUE, scale. = TRUE)

# REMOVE IN FUTURE. Check variance explained for each pca
city_pca %>% broom::tidy("pcs")

# Putting the non-numeric columns back
city_pca_augment <- city_pca %>% 
  broom::augment(city_df)

city_pca_augment %>%
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             color = division)) +
  geom_point()

############################ PCA ########################

# PCA of regional data
province_pca <- province_df %>%
  select_at(vars(elementary_school_count:nursing_home_count)) %>% 
  prcomp(center = TRUE, scale. = TRUE)

# REMOVE IN FUTURE. Check variance explained for each pca
province_pca %>% broom::tidy("pcs")

# Putting the non-numeric columns back
province_pca_augment <- province_pca %>% 
  broom::augment(province_df)

province_pca_augment %>%
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             color = deceased,
             size = deceased)) +
  geom_point() +
  guides(size = FALSE)

