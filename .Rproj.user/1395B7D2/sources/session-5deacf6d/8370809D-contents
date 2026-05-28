# =========================================
# ADVANCED COVID-19 DASHBOARD
# =========================================

# Load libraries
library(tidyverse)
library(scales)
library(patchwork)
library(cowplot)

# Load data
covid_data <- read_csv("outputs/final_covid_cleaned_data.csv")

# =========================================
# KPI VALUES
# =========================================

total_confirmed <- sum(covid_data$confirmed_cases, na.rm = TRUE)

total_deaths <- sum(covid_data$death_cases, na.rm = TRUE)

total_recovered <- sum(covid_data$recovered_cases, na.rm = TRUE)

# =========================================
# KPI CARD FUNCTION
# =========================================

create_kpi <- function(title, value) {
  
  ggplot() +
    
    annotate(
      "text",
      x = 0.5,
      y = 0.6,
      label = comma(value),
      size = 10,
      fontface = "bold"
    ) +
    
    annotate(
      "text",
      x = 0.5,
      y = 0.3,
      label = title,
      size = 5
    ) +
    
    theme_void() +
    
    theme(
      plot.background = element_rect(
        fill = "gray95",
        color = "gray80",
        linewidth = 1
      )
    )
}

kpi1 <- create_kpi("Total Confirmed", total_confirmed)

kpi2 <- create_kpi("Total Deaths", total_deaths)

kpi3 <- create_kpi("Total Recovered", total_recovered)

# =========================================
# TOP COUNTRIES
# =========================================

top_confirmed <- covid_data %>%
  group_by(country_region) %>%
  summarise(
    confirmed = sum(confirmed_cases, na.rm = TRUE)
  ) %>%
  arrange(desc(confirmed)) %>%
  slice_head(n = 10)

p1 <- ggplot(
  top_confirmed,
  aes(
    x = reorder(country_region, confirmed),
    y = confirmed
  )
) +
  
  geom_col(fill = "steelblue") +
  
  geom_text(
    aes(label = comma(confirmed)),
    hjust = -0.1,
    size = 3.5
  ) +
  
  coord_flip() +
  
  labs(
    title = "Top 10 Countries",
    x = "",
    y = "Confirmed Cases"
  ) +
  
  scale_y_continuous(labels = comma) +
  
  theme_minimal()

# =========================================
# GLOBAL TREND
# =========================================

trend_data <- covid_data %>%
  group_by(dates) %>%
  summarise(
    confirmed = sum(confirmed_cases, na.rm = TRUE)
  )

p2 <- ggplot(
  trend_data,
  aes(x = dates, y = confirmed)
) +
  
  geom_line(color = "darkblue", linewidth = 1) +
  
  labs(
    title = "Global COVID-19 Trend",
    x = "Date",
    y = "Cases"
  ) +
  
  scale_y_continuous(labels = comma) +
  
  theme_minimal()

# =========================================
# MONTHLY TREND
# =========================================

monthly_data <- covid_data %>%
  mutate(month = format(dates, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(
    confirmed = sum(confirmed_cases, na.rm = TRUE)
  )

p3 <- ggplot(
  monthly_data,
  aes(x = month, y = confirmed, group = 1)
) +
  
  geom_line(color = "darkgreen", linewidth = 1) +
  
  geom_point(size = 2) +
  
  labs(
    title = "Monthly COVID Trend",
    x = "Month",
    y = "Cases"
  ) +
  
  scale_y_continuous(labels = comma) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# =========================================
# RECOVERY RATE
# =========================================

recovery_data <- covid_data %>%
  group_by(country_region) %>%
  summarise(
    confirmed = sum(confirmed_cases, na.rm = TRUE),
    recovered = sum(recovered_cases, na.rm = TRUE)
  ) %>%
  mutate(
    recovery_rate = (recovered / confirmed) * 100
  ) %>%
  arrange(desc(recovery_rate)) %>%
  slice_head(n = 10)

p4 <- ggplot(
  recovery_data,
  aes(
    x = reorder(country_region, recovery_rate),
    y = recovery_rate
  )
) +
  
  geom_col(fill = "purple") +
  
  geom_text(
    aes(label = round(recovery_rate, 1)),
    hjust = -0.1,
    size = 3.5
  ) +
  
  coord_flip() +
  
  labs(
    title = "Top Recovery Rates",
    x = "",
    y = "Recovery %"
  ) +
  
  theme_minimal()

# =========================================
# COMBINE DASHBOARD
# =========================================

dashboard <-
  
  (kpi1 | kpi2 | kpi3) /
  
  (p1 | p2) /
  
  (p3 | p4)

# =========================================
# TITLE
# =========================================

dashboard <- dashboard +
  
  plot_annotation(
    title = "COVID-19 Advanced Analytics Dashboard",
    subtitle = "Professional Portfolio Project in R",
    theme = theme(
      plot.title = element_text(
        size = 22,
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = 12
      )
    )
  )

# =========================================
# DISPLAY
# =========================================

dashboard

# =========================================
# SAVE
# =========================================

ggsave(
  "plots/advanced_covid_dashboard.png",
  dashboard,
  width = 20,
  height = 14,
  dpi = 300
)