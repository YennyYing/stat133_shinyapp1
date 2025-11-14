# Title:California Crash Explorer (2021–2023)
# Description:An interactive Shiny app that visualizes California traffic crash data (2021–2023) through crash maps, county-level choropleths, and exploratory trend graphics.
# Author: Yuan Ying
# Date:05/02/2025
# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)  # data wrangling and graphics
library(maps)       # includes map of USA counties
library(leaflet)    # web interactive maps
library(plotly)     # web interactive graphics
library(sf)

# =======================================================
# Import data
#
# For demo purposes, in this "template" we use storms data
# (but you will have to replace this with the crash data)
#
# Uncomment the lines below and fill in the missing arguments to import the data!!!
# We're assuming the CSV file is in the working directory of your app.
# =======================================================
crashes <- read_csv(
  file = "crashes_2021_2023.csv", 
  col_types = cols(
    CASE_ID = col_double(),
    ACCIDENT_YEAR = col_double(),
    COLLISION_DATE = col_date(format = ""),
    COLLISION_TIME = col_double(),
    HOUR = col_double(),
    DAY_OF_WEEK = col_double(),
    COLLISION_SEVERITY = col_character(),
    NUMBER_KILLED = col_double(),
    NUMBER_INJURED = col_double(),
    VIOLATION_CATEGORY = col_character(),
    TYPE_OF_COLLISION = col_character(),
    PEDESTRIAN_ACCIDENT = col_character(),
    BICYCLE_ACCIDENT = col_character(),
    MOTORCYCLE_ACCIDENT = col_character(),
    TRUCK_ACCIDENT = col_character(),
    COUNTY = col_character(),
    CITY = col_character(),
    LOCATION = col_character(),
    POINT_X = col_double(),
    POINT_Y = col_double()
  )
)
# =======================================================
# Map of California counties ("sf" object)
#
# st_transform() is used to prevent leaflet from giving a 
# warning since the county polygons are in low scale
# =======================================================

cal_counties_sf <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) |>
  mutate(state = str_extract(ID, "\\w+"),
         county = str_extract(ID, "(?<=,)\\s*.*") |> str_trim()) |>
  filter(state == "california") |>
  st_transform(crs = 4326)


# =======================================================
# Storms data: This is just for demo purposes 
# (delete or comment these commands).
# To be clear: You'll have to use the crash data
# =======================================================
#storms = storms |>
#  mutate(id = paste0(name, "-", year))


# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  titlePanel("California Crash Explorer (2021–2023)"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Map with Location of Crashes"),
        selectInput("year", "Year", choices = sort(unique(crashes$ACCIDENT_YEAR))),
        selectInput("location", "Location", choices = sort(unique(crashes$LOCATION)),
                    selected = unique(crashes$LOCATION)[1], multiple = TRUE),
        selectInput("violation", "Violation Category", choices = unique(crashes$VIOLATION_CATEGORY),
                    selected = unique(crashes$VIOLATION_CATEGORY)[1], multiple = TRUE),
        radioButtons("color_by", "Color by:",
                     choices = c("Type of Collision" = "TYPE_OF_COLLISION",
                                 "Severity" = "COLLISION_SEVERITY"))
      ),
      conditionalPanel(
        condition = "input.tabselected==2",
        h4("Choropleth Map"),
        selectInput("choropleth_year", "Select Year:",
                    choices = sort(unique(crashes$ACCIDENT_YEAR)),
                    selected = max(crashes$ACCIDENT_YEAR))
      ),
      conditionalPanel(
        condition = "input.tabselected==3",
        h4("Exploratory Analysis"),
        selectInput("plot_year", "Year:", choices = sort(unique(crashes$ACCIDENT_YEAR))),
        selectInput("plot_county", "County:", choices = sort(unique(crashes$COUNTY))),
        selectInput("plot_variable", "Variable:", 
                    choices = c("Type of Collision" = "TYPE_OF_COLLISION",
                                "Violation Category" = "VIOLATION_CATEGORY",
                                "Severity" = "COLLISION_SEVERITY"))
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Map 1", value = 1, leafletOutput("map1", height = 600)),
        tabPanel("Map 2", value = 2, leafletOutput("map2", height = 600)),
        tabPanel("More", value = 3,
                 plotlyOutput("plot1"), hr(),
                 plotOutput("plot2"), hr(),
                 plotOutput("plot3")),
        id = "tabselected"
      )
    )
  )
)

# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # ------------------------------------------------
  # Output for first TAB (i.e. map of crashes locations)
  # (adapt code to your analysis)
  # ------------------------------------------------
  output$map1 <- renderLeaflet({
    filtered <- crashes |>
      filter(ACCIDENT_YEAR == input$year,
             LOCATION %in% input$location,
             VIOLATION_CATEGORY %in% input$violation)
    
    pal <- colorFactor(rainbow(length(unique(filtered[[input$color_by]]))),
                       domain = filtered[[input$color_by]])
    
    leaflet(filtered) |>
      addTiles() |>
      addCircles(lng = ~POINT_X, lat = ~POINT_Y,
                 color = ~pal(get(input$color_by)),
                 label = ~paste0("Date: ", COLLISION_DATE,
                                 "<br>Type: ", TYPE_OF_COLLISION,
                                 "<br>Cause: ", VIOLATION_CATEGORY) |> lapply(HTML)) |>
      addLegend("bottomright", pal = pal, values = ~get(input$color_by),
                title = input$color_by)
  })
  
  output$map2 <- renderLeaflet({
    county_crash_count <- crashes |>
      filter(ACCIDENT_YEAR == input$choropleth_year) |>
      mutate(county_lower = tolower(COUNTY)) |>
      group_by(county_lower) |>
      summarise(n_crashes = n(), .groups = "drop")
    
    map_data <- cal_counties_sf |>
      left_join(county_crash_count, by = c("county" = "county_lower"))
    
    if (all(is.na(map_data$n_crashes))) {
      showNotification("No crash data for selected year.", type = "warning")
    }
    
    pal <- colorNumeric("YlOrRd", domain = map_data$n_crashes, na.color = "#eeeeee")
    
    leaflet(map_data) |>
      addTiles() |>
      addPolygons(
        fillColor = ~pal(n_crashes),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~paste0(
          str_to_title(county), ": ",
          ifelse(is.na(n_crashes), "No data", n_crashes), " crashes"
        ) |> lapply(HTML)
      ) |>
      addLegend("bottomright", pal = pal, values = ~n_crashes,
                title = paste("Crashes in", input$choropleth_year)) |>
      addControl("<strong>Crash Choropleth Map</strong>",
                 position = "topright",
                 className = "map-title")
  })
  
  output$plot1 <- renderPlotly({
    crash_summary <- crashes |>
      filter(ACCIDENT_YEAR == input$plot_year) |>
      count(DAY_OF_WEEK)
    
    p <- ggplot(crash_summary, aes(x = factor(DAY_OF_WEEK), y = n)) +
      geom_col(fill = "skyblue") +
      labs(title = paste("Number of Crashes by Day (", input$plot_year, ")", sep = ""),
           x = "Day of Week", y = "Number of Crashes") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$plot2 <- renderPlot({
    data <- crashes |>
      filter(ACCIDENT_YEAR == input$plot_year,
             COUNTY == input$plot_county)
    
    ggplot(data, aes_string(x = input$plot_variable)) +
      geom_bar(fill = "yellow") +
      coord_flip() +
      labs(title = paste("Distribution of", input$plot_variable, "in", input$plot_county),
           x = "", y = "Count") +
      theme_minimal()
  })
  output$plot3 <- renderPlot({
    facet_data <- crashes |>
      filter(ACCIDENT_YEAR == input$plot_year,
             COUNTY == input$plot_county)
    
    ggplot(facet_data, aes(x = TYPE_OF_COLLISION)) +
      geom_bar(fill = "#FFB347") +
      facet_wrap(~ COLLISION_SEVERITY) +
      labs(title = paste("Type of Collision Faceted by Severity in", input$plot_county),
           x = "Type of Collision", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
