# Clear Workspace ---------------------------------------------------------
rm(list = ls())
options(scipen=999)
# Load libraries ----------------------------------------------------------
library("tidyverse")
library("tidygraph")
library("ggraph")
library("igraph")

# Load functions ----------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
patient_df <- read_tsv(file = "data/patient_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

# each pair of infecting person - infected person and age group of infected
transmission_df <- patient_df %>% 
  select(infected_by, patient_id, age_group) %>% 
  drop_na() %>% 
  distinct()

# the persons who infected someone
infecting <- transmission_df %>%
  distinct(infected_by) %>%
  rename(patient_id = infected_by)

# the persons who were infected
infected <- transmission_df %>%
  distinct(patient_id)

#make df with age of patients
age_df <- patient_df %>% 
  select(patient_id, age_group)

# both infected and infecting persons have to be nodes. Add age to each node.
# consider dropping NAs and remove edges correspondingly
nodes <- full_join(infecting, infected) %>% 
  left_join(., age_df, by = "patient_id") %>% 
  distinct() 

edges <- transmission_df %>% 
  rename(from = infected_by, to = patient_id)

# Visualise data ----------------------------------------------------------
graph_obj <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE) %>% 
  as_tbl_graph()

transmission_plot <- ggraph(graph_obj, layout = "fr") + 
  geom_node_point(aes(colour = age_group), size=1.5) +
  geom_edge_link(arrow = arrow(length = unit(1, 'mm')),
                 start_cap = circle(0.5, 'mm'),
                 end_cap = circle(0.5, 'mm')) +
  theme_graph()

# Write plots and data to file --------------------------------------------
ggsave(filename = "results/04_plot.png", plot = transmission_plot)
#write_tsv(x = dat, path = "data/wrangled_dat.tsv")