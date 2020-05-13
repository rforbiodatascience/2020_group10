
# Clear workspace -------------------------------------------------------------
rm(list = ls())

# Load libraries---------------------------------------------------------------
library(tidyverse)

# Functions -------------------------------------------------------------------

# Imported file names to snake case format-------------------------------------
snake_case <- function(old_name) {
  new_name <- old_name %>%
    str_replace("([:lower:])([:upper:])", "\\1_\\2") %>%
    str_replace("_raw/", "") %>%
    str_replace(".csv", "") %>%
    str_to_lower(.)
  return(new_name)
}

# Get lower matrix triangle ---------------------------------------------------

get_lower_tri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Deceased_model --------------------------------------------------------------

deceased_model <- function(df) {
  lm(deceased_time_age ~ confirmed_time_age, data = df)
}

# Sigmoid model ---------------------------------------------------------------
sigm_model <- function(df) {
  nls(value ~ SSlogis(date, Asym, xmid, scal), data = df)
}

# Theme template --------------------------------------------------------------
theme_group10 <- theme(
  axis.title.x = element_text(size = 19),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 19),
  axis.text.y = element_text(size = 14),
  plot.title = element_text(size = 29),
  plot.caption = element_text(size = 11),
  plot.subtitle = element_text(size = 22)
)

