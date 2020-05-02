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
by_type <- patient_df %>% 
  filter(state == "released") %>%
  select(patient_id, state_date, confirmed_date, age_group, infection_case) %>% 
  drop_na() %>% 
  mutate(hospitalization = state_date - confirmed_date) %>% 
  distinct() %>% 
  group_by(infection_case, age_group) 

summarized <- summarise(by_type, 
                        count = n(),
                        sd =sd(hospitalization),
                        avg = as.numeric(mean(hospitalization))) %>%
              filter(., count > 5)

# investigate time from tested positive to last timepoint in isolation
by_type2 <- patient_df %>% 
  filter(state == "isolated") %>%
  select(patient_id, confirmed_date, age_group, infection_case) %>% 
  mutate(hospitalization = as.Date("2020-04-06") - confirmed_date) %>% 
  drop_na() %>% 
  distinct() %>% 
  group_by(infection_case, age_group) 


summarized2 <- summarise(by_type2, 
                        count = n(),
                        sd =sd(hospitalization),
                        avg = as.numeric(mean(hospitalization))) %>%
  

# investigate deaths
by_type3 <- patient_df %>% 
  select(patient_id, state, age_group, infection_case) %>% 
  distinct() %>% 
  group_by(infection_case, state, age_group) 


summarized3 <- summarise(by_type3, 
                        count = n()) %>% 
                filter(., count > 5)




# Visualise data ----------------------------------------------------------
# From confirmed to released
ggplot(summarized, aes(x=infection_case, y=avg, fill=age_group)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd), width=.9, size = 0.2,
                position=position_dodge2(width = 0.9, preserve = "single"))
# Time in isolation so far
ggplot(summarized2, aes(x=infection_case, y=avg, fill=age_group)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd), width=.9, size = 0.2,
                position=position_dodge2(width = 0.9, preserve = "single"))

# Deaths
ggplot(summarized3, aes(x=infection_case, y=count, fill=age_group)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Stacked
    ggplot(summarized3, aes(fill=state, y=count, x=infection_case)) + 
     geom_bar(position="fill", stat="identity")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Write plots and data to file --------------------------------------------
ggsave(filename = "results/04_plot.png", plot = )
write_tsv(x = , path = "data/wrangled_transmission.tsv")
