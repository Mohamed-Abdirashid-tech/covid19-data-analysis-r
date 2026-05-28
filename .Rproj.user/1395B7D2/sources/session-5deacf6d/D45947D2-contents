# =========================================
# COVID-19 EXPLORATORY DATA ANALYSIS
# =========================================

library(tidyverse)
library(lubridate)

# Load cleaned data
covid_data <- read_csv("outputs/final_covid_cleaned_data.csv")

# =========================================
# Top 10 countries by confirmed cases
# =========================================

colnames(covid_data)

top_confirmed <- covid_data %>%
  group_by(country_region) %>%
  summarise(total_confirmed = sum(confirmed_cases, na.rm = TRUE)) %>%
  arrange(desc(total_confirmed)) %>%
  slice_head(n = 10)

print(top_confirmed)

# =========================================
# Bar Chart - Top 10 Countries
# =========================================

ggplot(top_confirmed,
       aes(x = reorder(country_region, total_confirmed),
           y = total_confirmed)) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Countries by Confirmed COVID-19 Cases",
    x = "Country",
    y = "Confirmed Cases"
  )




ggsave(
  "plots/top_10_confirmed_cases.png",
  width = 10,
  height = 6
)