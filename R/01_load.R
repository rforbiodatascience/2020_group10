# Clear workspace -------------------------------------------------------------
rm(list = ls())

# Load libraries --------------------------------------------------------------
library(tidyverse)

# Define functions ------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load all dataset files of the Korean COVID19 Dataset ------------------------

# Get file paths for database table files
dataset_files <- list.files(path = "_raw/*.csv", full.names = TRUE)

# Load table files and set filename to snake case format
dataset_tables <- dataset_files %>%
  setNames(nm = snake_case(.)) %>%
  map(read_csv)

# Writing all tables to the data folder
dataset_tables %>%
  names(.) %>%
  map(~ write_tsv(pluck(dataset_tables, .), str_c("data/", ., "_data.tsv")))
