# =========================================
# PROFESSIONAL COVID-19 DASHBOARD
# =========================================

# Load libraries
library(tidyverse)
library(scales)
library(patchwork)

# Load dataset
covid_data <- read_csv("outputs/final_covid_cleaned_data.csv")

# =========================================
# SUMMARY DATA
# =========================================

confirmed_summary <- covid_data %>%
  group_by(country_region) %>%
  summarise(total_confirmed = sum(confirmed_cases, na.rm = TRUE)) %>%
  arrange(desc(total_confirmed)) %>%
  slice_head(n = 10)

death_summary <- covid_data %>%
  group_by(country_region) %>%
  summarise(total_deaths = sum(death_cases, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 10)

recovered_summary <- covid_data %>%
  group_by(country_region) %>%
  summarise(total_recovered = sum(recovered_cases, na.rm = TRUE)) %>%
  arrange(desc(total_recovered)) %>%
  slice_head(n = 10)

# =========================================
# COMMON THEME
# =========================================

custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# =========================================
# CHART 1 - CONFIRMED CASES
# =========================================

p1 <- ggplot(
  confirmed_summary,
  aes(
    x = reorder(country_region, total_confirmed),
    y = total_confirmed
  )
) +
  
  geom_col(fill = "steelblue") +
  
  geom_text(
    aes(label = comma(total_confirmed)),
    hjust = -0.1,
    size = 3.5
  ) +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Confirmed Cases",
    x = "",
    y = "Confirmed Cases"
  ) +
  
  scale_y_continuous(labels = comma) +
  
  custom_theme

# =========================================
# CHART 2 - DEATH CASES
# =========================================

p2 <- ggplot(
  death_summary,
  aes(
    x = reorder(country_region, total_deaths),
    y = total_deaths
  )
) +
  
  geom_col(fill = "darkred") +
  
  geom_text(
    aes(label = comma(total_deaths)),
    hjust = -0.1,
    size = 3.5
  ) +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Death Cases",
    x = "",
    y = "Deaths"
  ) +
  
  scale_y_continuous(labels = comma) +
  
  custom_theme

# =========================================
# CHART 3 - RECOVERED CASES
# =========================================

p3 <- ggplot(
  recovered_summary,
  aes(
    x = reorder(country_region, total_recovered),
    y = total_recovered
  )
) +
  
  geom_col(fill = "darkgreen") +
  
  geom_text(
    aes(label = comma(total_recovered)),
    hjust = -0.1,
    size = 3.5
  ) +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Recovered Cases",
    x = "",
    y = "Recovered"
  ) +
  
  scale_y_continuous(labels = comma) +
  
  custom_theme

# =========================================
# CHART 4 - GLOBAL TREND
# =========================================

trend_data <- covid_data %>%
  group_by(dates) %>%
  summarise(
    confirmed = sum(confirmed_cases, na.rm = TRUE)
  )

p4 <- ggplot(
  trend_data,
  aes(x = dates, y = confirmed)
) +
  
  geom_line(color = "blue", linewidth = 1) +
  
  labs(
    title = "Global COVID-19 Trend",
    x = "Date",
    y = "Confirmed Cases"
  ) +
  
  scale_y_continuous(labels = comma) +
  
  custom_theme

# =========================================
# COMBINE DASHBOARD
# =========================================

dashboard <- (p1 | p2) /
  (p3 | p4)

# Dashboard title
dashboard <- dashboard +
  plot_annotation(
    title = "COVID-19 Global Dashboard",
    subtitle = "Confirmed, Death, Recovery & Trend Analysis",
    theme = theme(
      plot.title = element_text(
        size = 20,
        face = "bold"
      ),
      plot.subtitle = element_text(size = 12)
    )
  )

# =========================================
# DISPLAY DASHBOARD
# =========================================

dashboard

# =========================================
# SAVE DASHBOARD
# =========================================

ggsave(
  "plots/professional_covid_dashboard.png",
  dashboard,
  width = 18,
  height = 12,
  dpi = 300
)


