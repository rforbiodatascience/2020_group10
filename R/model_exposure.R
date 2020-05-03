# Clear Workspace ---------------------------------------------------------
rm(list = ls())
options(scipen=999)

# Load libraries ----------------------------------------------------------
library("tidyverse")

# Load functions ----------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
patient_df <- read_tsv(file = "data/patient_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

# investigate time from tested positive to being released
by_type_released <- patient_df %>% 
  filter(state == "released") %>%
  select(patient_id, state_date, confirmed_date, age_group, infection_case) %>% 
  drop_na() %>% 
  mutate(hospitalization = state_date - confirmed_date) %>% 
  distinct() %>% 
  group_by(infection_case, age_group) 

summarized_released <- summarise(by_type_released, 
                        count = n(),
                        sd =sd(hospitalization),
                        avg = as.numeric(mean(hospitalization))) %>%
              filter(., count > 5)

# investigate time from tested positive to last timepoint in isolation
by_type_isolated <- patient_df %>% 
  filter(state == "isolated") %>%
  select(patient_id, confirmed_date, age_group, infection_case) %>% 
  mutate(hospitalization = as.Date("2020-04-06") - confirmed_date) %>% 
  drop_na() %>% 
  distinct() %>% 
  group_by(infection_case, age_group) 


summarized_isolated <- summarise(by_type_isolated, 
                        count = n(),
                        sd =sd(hospitalization),
                        avg = as.numeric(mean(hospitalization))) %>%
                       filter(., count > 5)
  
# Visualise data ----------------------------------------------------------
# From confirmed to released
ggplot(summarized_released, aes(x=infection_case, y=avg, fill=age_group)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd), width=.9, size = 0.2,
                position=position_dodge2(width = 0.9, preserve = "single"))
# Time in isolation so far
ggplot(summarized_isolated, aes(x=infection_case, y=avg, fill=age_group)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd), width=.9, size = 0.2,
                position=position_dodge2(width = 0.9, preserve = "single"))


# Write plots and data to file --------------------------------------------
#ggsave(filename = "results/04_plot.png", plot = )
#write_tsv(x = , path = "data/wrangled_transmission.tsv")
