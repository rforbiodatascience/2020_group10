# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggraph")

# Load functions ----------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
transmission_df <- read_tsv(file = "data/wrangled_transmission.tsv")

# Wrangle data ------------------------------------------------------------
# Find the persons who infect 10 or more others
filtered_transmission_df <- transmission_df %>%
  group_by(from) %>%
  filter(n() >= 10) %>%
  ungroup()

# Create nodes
nodes <- filtered_transmission_df %>%
  pivot_longer(
    cols = c(to, from),
    names_to = "status",
    values_to = "patient_id"
  ) %>%
  mutate(age_group = case_when(
    status == "from" ~ age_group_infecting,
    status == "to" ~ age_group_infected
  )) %>%
  select(patient_id, age_group) %>%
  distinct()

# Visualise data ----------------------------------------------------------
# Create graph object
graph_obj <- graph_from_data_frame(filtered_transmission_df,
  vertices = nodes, directed = TRUE
  ) %>%
  as_tbl_graph()

# Create plot with nodes and arrows as edges
zoom_transmission_plot <- ggraph(graph_obj, layout = "fr") +
  geom_edge_link(aes(colour = infection_case),
    arrow = arrow(length = unit(1.5, "mm")),
    start_cap = circle(1, "mm"),
    end_cap = circle(1, "mm"),
    edge_width = 0.4
  ) +
  geom_node_point(aes(colour = age_group), size = 2) +
  labs(
    title = "Transmission plot for COVID-19 for patients who infect more than 10 individuals",
    subtitle = "Patients are coloured by age group for patients and transmission events by route",
    color = "Age group",
    caption = "Data from Korea Centers for Disease Control & Prevention (2020)"
  ) +
  theme_graph(title_size = 16)

# Write plots and data to file --------------------------------------------
ggsave(
  filename = "results/17_subset_transmission.png",
  plot = zoom_transmission_plot, 
  width = 8, 
  height = 6
)
write_tsv(x = filtered_transmission_df, path = "data/wrangled_transmission_filtered.tsv")

# Detach external packages ---------------------------------------------------------------------
detach("package:ggraph", unload=TRUE)