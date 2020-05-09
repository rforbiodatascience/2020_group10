# Clear workspace -------------------------------------------------------------
rm(list = ls())

# Load libraries---------------------------------------------------------------
library(tidyverse)

# Functions -------------------------------------------------------------------

# Converts the name of imported tables to snake case format
snake_case <- function(old_name) {
  new_name <- old_name %>%
    str_replace("([:lower:])([:upper:])", "\\1_\\2") %>%
    str_replace("_raw/", "") %>%
    str_replace(".csv", "") %>%
    str_to_lower(.)
  return(new_name)
}