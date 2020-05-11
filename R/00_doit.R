# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Run scripts
# ------------------------------------------------------------------------------
options(scipen = 999)

source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_augment.R")
source(file = "R/05_model_case_type.R")
source(file = "R/06_model_cor.R")
source(file = "R/07_model_patient_route.R")
source(file = "R/08_model_city_pca.R")
source(file = "R/09_model_city_ann.R")
source(file = "R/10_model_most_visited.R")
source(file = "R/11_model_city_clustering.R")
source(file = "R/12_model_case_fatality_age.R")
source(file = "R/13_model_disease_progress.R")
source(file = "R/14_model_exposure.R")
source(file = "R/15_model_searchtrends.R")
source(file = "R/16_model_transmission.R")
source(file = "R/17_model_subset_transmission.R")