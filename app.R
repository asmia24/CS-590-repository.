library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)

ui <- navbarPage(
  "USGS Earthquake Dashboard",
  
  tabPanel(
    "Project Information",
    br(),
    h2("9-1 Project", align = "center"),
    h3("CS-590-10407-M01 Database Design & Development", align = "center"),
    h4("Professor Zach Probst, M.S.", align = "center"),
    h4("Ashanti Mia Terrell, MS, PMP, CSSBB", align = "center"),
    h4("June 21, 2026", align = "center"),
    br(),
    p(
      "This dashboard analyzes United States Geological Survey (USGS) earthquake data using PostgreSQL, RStudio, and Shiny. The dashboard provides visualizations of earthquake magnitude distributions, geographic event locations, and summary statistics."
    )
  ),
  
  tabPanel(
    "Magnitude Distribution",
    plotOutput("histPlot")
  ),
  
  tabPanel(
    "Earthquake Map",
    leafletOutput("map", height = 700)
  ),
  
  tabPanel(
    "Summary Statistics",
    br(),
    h3("Dataset Summary"),
    tableOutput("summary")
  )
)

server <- function(input, output) {
  
  output$histPlot <- renderPlot({
    ggplot(eq_data, aes(x = magnitude)) +
      geom_histogram(
        bins = 30,
        fill = "#B8D8E8",
        color = "white"
      ) +
      theme_minimal() +
      labs(
        title = "Earthquake Magnitude Distribution",
        x = "Magnitude",
        y = "Number of Earthquakes"
      )
  })
  
  output$map <- renderLeaflet({
    leaflet(eq_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~pmax(magnitude, 1),
        color = "#CDB4DB",
        fillOpacity = 0.6,
        popup = ~paste(
          "<b>Location:</b>", place,
          "<br><b>Magnitude:</b>", magnitude,
          "<br><b>Depth:</b>", depth_km, "km",
          "<br><b>Time:</b>", event_time
        )
      )
  })
  
  output$summary <- renderTable({
    data.frame(
      Metric = c(
        "Total Earthquakes",
        "Maximum Magnitude",
        "Average Magnitude",
        "Deepest Earthquake (km)",
        "Tsunami Events"
      ),
      Value = c(
        nrow(eq_data),
        max(eq_data$magnitude, na.rm = TRUE),
        round(mean(eq_data$magnitude, na.rm = TRUE), 2),
        max(eq_data$depth_km, na.rm = TRUE),
        sum(eq_data$tsunami == 1, na.rm = TRUE)
      )
    )
  })
}

shinyApp(ui = ui, server = server)
