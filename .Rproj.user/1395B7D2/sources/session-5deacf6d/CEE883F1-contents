library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(scales)

# Load data
covid_data <- read_csv("../outputs/final_covid_cleaned_data.csv")

# Summary data
top_confirmed <- covid_data %>%
  group_by(country_region) %>%
  summarise(
    confirmed = sum(confirmed_cases, na.rm = TRUE)
  ) %>%
  arrange(desc(confirmed)) %>%
  slice_head(n = 10)

trend_data <- covid_data %>%
  group_by(dates) %>%
  summarise(
    confirmed = sum(confirmed_cases, na.rm = TRUE)
  )

# UI
ui <- dashboardPage(
  
  skin = "black",
  
  dashboardHeader(
    title = "COVID-19 Dashboard"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard")
    )
  ),
  
  dashboardBody(
    
    fluidRow(
      
      valueBoxOutput("confirmedBox", width = 4),
      valueBoxOutput("deathBox", width = 4),
      valueBoxOutput("recoveredBox", width = 4)
      
    ),
    
    fluidRow(
      
      box(
        title = "Top 10 Countries",
        width = 6,
        plotlyOutput("topCountriesPlot")
      ),
      
      box(
        title = "Global Trend",
        width = 6,
        plotlyOutput("trendPlot")
      )
      
    )
  )
)

# SERVER
server <- function(input, output) {
  
  output$confirmedBox <- renderValueBox({
    
    valueBox(
      comma(sum(covid_data$confirmed_cases)),
      "Total Confirmed",
      icon = icon("virus"),
      color = "blue"
    )
  })
  
  output$deathBox <- renderValueBox({
    
    valueBox(
      comma(sum(covid_data$death_cases)),
      "Total Deaths",
      icon = icon("skull"),
      color = "red"
    )
  })
  
  output$recoveredBox <- renderValueBox({
    
    valueBox(
      comma(sum(covid_data$recovered_cases)),
      "Total Recovered",
      icon = icon("heart"),
      color = "green"
    )
  })
  
  output$topCountriesPlot <- renderPlotly({
    
    p <- ggplot(
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
        size = 3
      ) +
      
      coord_flip() +
      
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$trendPlot <- renderPlotly({
    
    p <- ggplot(
      trend_data,
      aes(x = dates, y = confirmed)
    ) +
      
      geom_line(color = "cyan", linewidth = 1) +
      
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run app
shinyApp(ui, server)



install.packages("leaflet")
install.packages("sf")






leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~long,
    lat = ~lat,
    radius = ~confirmed/10000,
    popup = ~country_region
  )