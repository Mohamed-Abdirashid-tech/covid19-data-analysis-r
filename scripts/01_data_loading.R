# =========================================
# COVID-19 DATA ANALYSIS PROJECT
# Data Loading & Inspection
# =========================================

library(tidyverse)
library(janitor)
library(lubridate)


covid_data <- read_csv("data/covid19_timeseries.csv")


head(covid_data)

glimpse(covid_data)

colnames(covid_data)

dim(covid_data)



colSums(is.na(covid_data))



#CLEANING COLUMN NAMES


covid_data <- clean_names(covid_data)

colnames(covid_data)


write_csv(covid_data, "outputs/cleaned_covid_data.csv")
