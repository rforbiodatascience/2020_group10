# Clear workspace ---------------------------------------------------------------
rm(list = ls())

# Load libraries ---------------------------------------------------------------
library("tidyverse")
library("keras")

# Define functions ---------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
city_conf_pca_aug <- read_tsv("data/wrangled_city_pca.tsv")

# Wrangle data ---------------------------------------------------------------

# Convert classes to numeric
nn_dat <- city_conf_pca_aug %>%
  mutate(class_label = as.factor(class)) %>%
  mutate(
    class_num = as.numeric(as.factor(class)) - 1,
    class_label = class
  )

# Define test size
test_f <- 0.20

# Create partion of the data
nn_dat <- nn_dat %>%
  mutate(partition = sample(
    x = c("train", "test"),
    size = nrow(.),
    replace = TRUE,
    prob = c(1 - test_f, test_f)
  ))

# Define the training sets
x_train <- nn_dat %>%
  filter(partition == "train") %>%
  select(elementary_school_count:nursing_home_count) %>%
  as.matrix()

y_train <- nn_dat %>%
  filter(partition == "train") %>%
  pull(class_num) %>%
  to_categorical(4)

# Define the test sets
x_test <- nn_dat %>%
  filter(partition == "test") %>%
  select(elementary_school_count:nursing_home_count) %>%
  as.matrix()

y_test <- nn_dat %>%
  filter(partition == "test") %>%
  pull(class_num) %>%
  to_categorical(4)

# Define model and compile -------------------------------------------------

# 1 hidden layer; 9 neurons
model <- keras_model_sequential() %>%
  layer_dense(units = 9, activation = "relu", input_shape = 7) %>%
  layer_dense(units = 4, activation = "softmax")

model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = c("accuracy")
  )

# Fit model ---------------------------------------------------------------

history <- model %>%
  fit(x = x_train,
      y = y_train,
      epochs = 50,
      batch_size = 5,
      validation_split = 0
  )

# Performance of the model ---------------------------------------------------------------

perf <- model %>% 
  evaluate(x_test, y_test) %>%
  as_tibble()

# Save model performance  ---------------------------------------------------------------

write_tsv(perf, "data/wrangled_ann_pred")

# Detach external packages ---------------------------------------------------------------------
detach("package:keras", unload=TRUE)