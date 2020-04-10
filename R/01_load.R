#install.packages("tidyverse")
library("tidyverse")
#install.packages("dplyr")
library("dplyr")
options(scipen = 999)
### Load relevant csv files from _raw folder ###

# If anyone can find out how to import all files (like Leon did) and extract them from 
# the list so we can do different joins, feel free to do so :) 
setwd("/cloud/project/_raw")

Time <- read_csv(file = "Time.csv")
TimeAge <- read_csv(file = "TimeAge.csv")
TimeGender <- read_csv(file = "TimeGender.csv")
TimeProvince <- read_csv(file = "TimeProvince.csv")

Region <- read_csv(file = "Region.csv")
SearchTrend <- read_csv(file="SearchTrend.csv")

PatientInfo <- read_csv(file = "PatientInfo.csv")
PatientRoute <- read_csv(file = "PatientRoute.csv")
Case <-  read_csv(file = "Case.csv")


# Join the time data in the different tables
# First change naming so we do not have double names
Time <- Time %>% rename_at(vars(-date, -time), ~ paste0(., '_total'))
TimeProvince <- TimeProvince %>% rename_at(vars(-date, -time, -province), ~ paste0(., '_province'))

TimeAge <- TimeProvince %>% rename_at(vars(-date, -time, -age), ~ paste0(., '_age'))
TimeGender <- TimeProvince %>% rename_at(vars(-date, -time, -sex), ~ paste0(., '_gender'))
time_frame <- TimeProvince %>% 
  left_join(Time, by=c("date","time")) %>% 
  left_join(TimeAge, by=c("date","time")) %>% 
  left_join(TimeGender, by = c("date","time"))
#join region and search data

patient_frame <- PatientInfo %>% 
  left_join(PatientRoute, by=c("patient_id","global_num", "province", "city"))

# I don't know if it makes sense to join with case before that one has been cleaned?
# join case on infection case w patien_table  
