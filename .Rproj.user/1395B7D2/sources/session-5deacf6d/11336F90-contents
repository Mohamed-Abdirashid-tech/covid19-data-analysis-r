# =========================================
# COVID-19 DATA CLEANING
# =========================================


library(tidyverse)
library(janitor)
library(lubridate)

covid_data <- read_csv("outputs/cleaned_covid_data.csv")


head(covid_data$dates)

covid_data$dates <- ymd(covid_data$dates)


glimpse(covid_data)


sum(duplicated(covid_data))

colSums(is.na(covid_data))

summary(covid_data)




# =========================================
# Convert dates column to proper date format
# =========================================

covid_data$dates <- mdy(covid_data$dates)

head(covid_data$dates)

# Check date conversion
glimpse(covid_data)

# =========================================
# Check duplicates
# =========================================

sum(duplicated(covid_data))

# =========================================
# Missing values check
# =========================================

colSums(is.na(covid_data))

# =========================================
# Replace missing province/state values
# =========================================

covid_data$province_state[is.na(covid_data$province_state)] <- "Unknown"

# Verify replacement
colSums(is.na(covid_data))

# =========================================
# Summary statistics
# =========================================


head(covid_data)

summary(covid_data)

# =========================================
# Save final cleaned dataset
# =========================================

write_csv(covid_data, "outputs/final_covid_cleaned_data.csv")
