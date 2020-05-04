# Clear Workspace ---------------------------------------------------------
rm(list = ls())
options(scipen=999)

# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load functions ----------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
time_df <- read_tsv(file = "data/time_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------
time_df_3 <- time_df %>% 
  select(date, coronavirus, confirmed) %>% 
  filter(date >= as.Date("2020-01-01")) %>% 
  distinct() %>% 
  mutate(confirmed2 = confirmed - lag(confirmed)) %>%
  pivot_longer(., cols = c(confirmed, confirmed2, coronavirus), names_to = "parameter", values_to = "value") 


time_df_2 <- time_df %>% 
  select(date, cold, flu, pneumonia, coronavirus) %>% 
  distinct() %>% 
  drop_na()


time_df_longer <- time_df %>% 
  select(date, cold, flu, pneumonia, coronavirus) %>% 
  distinct() %>% 
  drop_na() %>% 
  pivot_longer(., cols = c(cold, flu, pneumonia, coronavirus), names_to = "search_term", values_to = "value")

# Visualise data ----------------------------------------------------------
ggplot(time_df_3, aes(date)) +
  geom_line(aes(y = value)) +
  facet_wrap(~ parameter, scales="free_y", nrow = 3) 

ggplot(time_df_longer, aes(date)) + 
  geom_line(aes(y = value)) +
  facet_wrap(~ search_term, scales="free_y") 

ggplot(time_df_2, aes(date)) + 
  geom_line(aes(y = cold, colour = "cold")) +
  geom_line(aes(y = flu, colour = "flu")) +
  geom_line(aes(y = pneumonia, colour = "pneumonia")) +
  geom_line(aes(y = coronavirus, colour = "coronavirus")) +
  ylab("Search trend (relative)") 





, colour = "cold")) + 
  geom_line(aes(y = pneumonia, colour = "pneumonia")) +
  geom_line(aes(y = coronavirus, colour = "coronavirus")) +
  geom_line(aes(y = flu, colour = "flu")) +
  ylab("Search trend (relative)") 
  #geom_line(aes(y = confirmed, colour = "confimed COVID-19 cases"))
# Write plots and data to file --------------------------------------------
ggsave(filename = "results/exposure_plot.png", plot = exposure_plot)
write_tsv(x = summarized_by_type, path = "data/wrangled_exposure.tsv")
