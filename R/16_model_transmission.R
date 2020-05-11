# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggraph)
library(igraph)
library(tidygraph)

# Load functions ----------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
patient_df <- read_tsv(file = "data/patient_data_augmented.tsv")

# Wrangle data ------------------------------------------------------------

# Each pair of infecting person (from) and infected person (to) with both age groups
transmission_df <- patient_df %>%
  select(infected_by, patient_id, age_group, infection_case) %>%
  rename(
    age_group_infected = age_group,
    to = patient_id,
    from = infected_by
  ) %>%
  left_join(select(patient_df, patient_id, age_group), by = c("from" = "patient_id")) %>%
  rename(age_group_infecting = age_group) %>%
  drop_na() %>%
  distinct()

# Both infected and infecting persons have to be nodes.
nodes <- transmission_df %>%
  pivot_longer(
    cols = c(to, from),
    names_to = "status",
    values_to = "patient_id"
  )

# Add age to each node.
nodes <- nodes %>%
  mutate(age_group = case_when(
    status == "from" ~ age_group_infecting,
    status == "to" ~ age_group_infected
  )) %>%
  select(patient_id, age_group) %>%
  distinct()

# Visualise data ----------------------------------------------------------
# Create graph object
graph_obj <- graph_from_data_frame(transmission_df, vertices = nodes, directed = TRUE) %>%
  as_tbl_graph()

# Plot nodes and arrows as edges
transmission_plot <- ggraph(graph_obj, layout = "fr") +
  geom_node_point(aes(colour = age_group), size = 1.8) +
  geom_edge_link(
    arrow = arrow(length = unit(1, "mm")),
    start_cap = circle(0.5, "mm"),
    end_cap = circle(0.3, "mm"),
    edge_width = 0.2
  )

# Add labels and theme
transmission_plot <- transmission_plot +
  labs(
    title = "Transmission network for COVID-19 patients in South Korea",
    subtitle = "Stratified on age group for patients",
    color = "Age group",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  ) +
  theme_graph()

# Write plots and data to file --------------------------------------------
ggsave(
  filename = "results/16_transmission.png",
  plot = transmission_plot,
  width = 8,
  height = 6
)

write_tsv(x = transmission_df, path = "data/wrangled_transmission.tsv")

# Detach external packages ---------------------------------------------------------------------
detach("package:ggraph", unload=TRUE)