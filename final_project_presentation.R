## ----setup, include=FALSE-------------------------------------------------------------------------------------------
library(knitr)
library(tidyverse)
knitr::opts_chunk$set(echo = FALSE)


## ----out.width = "60%", fig.align='center',fig.cap= "COVID-19 dataset from kaggle"----------------------------------
include_graphics('dataset.png')


## ---- fig.width = 1, fig.align='center'-----------------------------------------------------------------------------
include_graphics('project_structure.png')
#include_graphics('images/workflow.png')


## ---- fig.width = 1, fig.align='center'-----------------------------------------------------------------------------
include_graphics('/doc/case_df.png')


## ---- fig.width = 1, fig.align='center'-----------------------------------------------------------------------------
include_graphics('patient_df.png')


## ---- fig.width = 1, fig.align='center'-----------------------------------------------------------------------------
include_graphics('time_df.png')


## ---- fig.width = 1, fig.align='center'-----------------------------------------------------------------------------
include_graphics('city_df.png')


## ----out.width = "50%", fig.show = "center"-------------------------------------------------------------------------
include_graphics(c('../results/10_most_visited_places.png', '../results/05_case_type.png'))


## ----out.width = "65%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/13_disease_progress.png')


## ----out.width = "75%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/13_logistic_model.png')


## ----out.width = "100%", fig.align = "center"-----------------------------------------------------------------------
include_graphics('../results/15_searchtrend.png')


## ----out.width = "70%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/13_disease_age.png')


## ----out.width = "50%"----------------------------------------------------------------------------------------------
include_graphics(c('../results/13_confirmed_gender.png', '../results/13_deceased_gender.png'))


## ----out.width = "55%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/12_age_deceased_ratio.png')


## ----out.width = "80%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/14_contact_exposure.png')


## ----out.width = "90%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/16_transmission.png')


## ----out.width = "55%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/08_city_pca.png')


## ----out.width = "90%", fig.align = "center", message=FALSE---------------------------------------------------------
include_graphics('../results/11_city_clustering.png')
cluster_pred <- read_tsv("../data/wrangled_cluster_pred.tsv") %>% 
  head()
knitr::kable(cluster_pred, format="html")


## -------------------------------------------------------------------------------------------------------------------
ann_pred <- read_tsv("../data/wrangled_ann_pred") %>% 
  head()
knitr::kable(ann_pred, format="html")


## ----out.width = "70%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/17_subset_transmission.png')


## ----out.width = "70%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/06_cor_heatmap.png')


## ----out.width = "55%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/08_city_pca_variance.png')


## ----out.width = "75%", fig.align = "center"------------------------------------------------------------------------
include_graphics('../results/07_cases_number.png')

