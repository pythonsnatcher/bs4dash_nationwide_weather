library(DBI)
library(RSQLite)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(bs4Dash)
library(lubridate)
library(ggplot2)
library(plotly)

# Connect to the SQLite database
con <- dbConnect(SQLite(), "/Users/snatch./PycharmProjects/nationwide_weather/nationwide_weather.db")

# Helper function to clean and filter weather data
cleanWeatherData <- function(weather_reports) {
  weather_reports %>%
    mutate(
      high_temperature = as.numeric(gsub("[^0-9.]", "", as.character(high_temperature))),
      wind_speed = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(wind_speed)))),
      current_temperature = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(current_temperature)))),
      low_temperature = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(low_temperature)))),
      low_tide_morning_height = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(low_tide_morning_height)))),
      high_tide_morning_height = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(high_tide_morning_height)))),
      low_tide_evening_height = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(low_tide_evening_height)))),
      high_tide_evening_height = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(high_tide_evening_height)))),
      time_of_search = as.POSIXct(time_of_search, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ) %>%
    filter(current_temperature != 0)  # Filter out rows where current_temperature is exactly zero
}


# Function to join and clean data
prepareData <- function(con) {
  # Load the data from the database
  weather_reports <- dbGetQuery(con, "SELECT * FROM WeatherReports")
  weather_conditions <- dbGetQuery(con, "SELECT * FROM WeatherConditions")
  locations <- dbGetQuery(con, "SELECT * FROM Locations")
  visibility_levels <- dbGetQuery(con, "SELECT * FROM VisibilityLevels")
  wind_directions <- dbGetQuery(con, "SELECT * FROM WindDirections")
  uv_index_levels <- dbGetQuery(con, "SELECT * FROM UVIndexLevels")
  pollen_levels <- dbGetQuery(con, "SELECT * FROM PollenLevels")
  pollution_levels <- dbGetQuery(con, "SELECT * FROM PollutionLevels")
  
  # Clean the weather data
  weather_reports <- cleanWeatherData(weather_reports)
  
  # Join the data
  joined_data <- weather_reports %>%
    left_join(weather_conditions, by = "weather_condition_id") %>%
    left_join(locations, by = "location_id") %>%
    left_join(visibility_levels, by = "visibility_id") %>%
    left_join(wind_directions, by = "wind_direction_id") %>%
    left_join(uv_index_levels, by = "uv_index_id") %>%
    left_join(pollen_levels, by = "pollen_id") %>%
    left_join(pollution_levels, by = "pollution_id")
  
  return(joined_data)
}

# Prepare the data for use in the Shiny app
data <- prepareData(con)

# Extract location names for use in the Shiny app
location_names <- data %>%
  select(name) %>%
  distinct() %>%
  pull()

# Disconnect from the database when the app is stopped
onStop(function() {
  dbDisconnect(con)
})

# UI definition
ui <- dashboardPage(
  dark = FALSE,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  title = "UK Weather Stats",
  
  header = dashboardHeader(
    title = dashboardBrand(
      title = 'UK Weather Stats',
      image = ''
    ),
    rightUi = dropdownMenu(
      badgeStatus = 'info',
      type = 'notifications',
      notificationItem(
        text = 'Success',
        status = 'success',
        icon = icon('circle-check')
      )
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = 'sidebarmenuid',
      menuItem('Home', tabName = 'home', icon = icon('home')),
      menuItem('Dashboard', tabName = 'dashboard', icon = icon('bar-chart')),
      conditionalPanel(
        condition = "input.sidebarmenuid === 'dashboard'",
        selectInput(
          inputId = 'location', 
          label = 'Select Location', 
          choices = location_names, 
          selected = if (length(location_names) > 0) location_names[1] else NULL
        )
      ),
      menuItem('Dataset', tabName = 'dataset', icon = icon('database'))
    )
  ),
  
  footer = dashboardFooter(
    left = 'Snatch',
    right = tags$div(
      tags$a(
        href = 'https://www.linkedin.com/in/chris-s-gilmour/',
        icon('linkedin', lib = "font-awesome", class = "fa-2x"),  # Larger LinkedIn icon
        title = 'LinkedIn Profile',
        target = '_blank',
        style = "margin-right: 10px; color: black;"  # Black color for LinkedIn icon
      ),
      tags$a(
        href = 'https://github.com/pythonsnatcher',
        icon('github', lib = "font-awesome", class = "fa-2x"),  # Larger GitHub icon
        title = 'GitHub Profile',
        target = '_blank',
        style = "color: black;"  # Black color for GitHub icon
      ),
      ' 2024'
    )
  ),
  
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = 'home',
        jumbotron(
          title = 'Weather Stats from Various Locations in England',
          status = 'info',
          lead = '',
          btnName = 'Download',
          href = 'https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bird-survey-results-for-areas-in-the-city-of-melbourne-february-and-march-2018/exports/csv?lang=en&timezone=Europe%2FLondon&use_labels=true&delimiter=%2C',
          'The dataset used in this visualization is available to download from my GitHub here'
        )
      ),
      
      tabItem(
        tabName = 'dashboard',
        fluidRow(
          column(
            width = 4,
            infoBox(
              width = 12,
              title = 'Variables Captured',
              value = textOutput('variable_count'),
              icon = icon('list')
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              title = 'Time Span',
              value = textOutput('time_diff'),
              icon = icon('hourglass')
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              title = 'Total Locations',
              value = textOutput('unique_locations'),
              icon = icon('location-dot')
            )
          )
        ),
        fluidRow(
          sortable(
            width = 6,
            box(
              title = "Wind Speed Histogram",
              width = 12, 
              status = "olive",
              plotOutput("windHistogram"),
              collapsed = TRUE,
              collapsible = TRUE
            ),
            
            box(
              title = "Current Temperature Time Series",
              width = 12, 
              status = 'olive',
              collapsed = TRUE,
              collapsible = TRUE,
              
              plotOutput("timeSeriesPlot")
            )
            
          ),
          sortable(
            width = 6,
            box(
              title = "Bird Sightings by Location",
              width = 12,  
              status = "olive",
              collapsed = TRUE,
              collapsible = TRUE,
              maximizable = TRUE
            ),
            box(
              title = "Total Sightings For Each Bird",
              width = 12, 
              status = "olive",
              collapsed = TRUE,
              collapsible = TRUE
            )
          )
        ),
        tabBox(
          title = 'Data',
          width = 12,
          type = 'tabs',
          status = 'olive',
          solidHeader = TRUE,
          tabPanel(
            'Site Locations',
            DTOutput('data_summary')
          ),
          tabPanel(
            'Birds',
            DTOutput('fk_relationships')
          )
        )
      ),
      
      tabItem(
        tabName = 'dataset',
        fluidRow(
          column(
            width = 12,
            tabBox(
              title = 'Data Overview',
              width = 12,
              type = 'tabs',
              status = 'olive',
              solidHeader = TRUE,
              tabPanel(
                'Data Summary',
                DTOutput('data_summary')
              ),
              tabPanel(
                'Foreign Key Relationships',
                DTOutput('fk_relationships')
              )
            )
          )
        )
      )
    )
  )
)

# Server function
server <- function(input, output, session) {
  # Function to generate a summary of data for each table using R's default summary function
  getDataSummary <- function(con) {
    # Get all table names from the database
    tables <- dbListTables(con)
    
    # Iterate through each table to get a summary of the data
    summaries <- lapply(tables, function(table) {
      data <- dbGetQuery(con, paste("SELECT * FROM", table))
      summary_data <- summary(data)
      return(list(table = table, summary = summary_data))
    })
    
    return(summaries)
  }
  #------------------------
  # Function to create a summary of foreign key relationships
  getFKRelationships <- function(con) {
    tables <- dbListTables(con)
    fk_relationships <- lapply(tables, function(table) {
      fk_constraints <- dbGetQuery(con, paste(
        "PRAGMA foreign_key_list(", table, ")"
      ))
      return(fk_constraints)
    })
    
    return(fk_relationships)
  }
  #-------------------
  # Load and clean data
  weather_reports <- dbGetQuery(con, "SELECT * FROM WeatherReports")
  wind_directions <- dbGetQuery(con, "SELECT * FROM WindDirections")
  locations <- dbGetQuery(con, "SELECT * FROM Locations")
  
  # Clean weather data
  weather_reports <- cleanWeatherData(weather_reports)
  
  # Join data
  joined_data <- weather_reports %>%
    left_join(wind_directions, by = "wind_direction_id") %>%
    left_join(locations, by = "location_id")
  
  # Update location choices
  updateSelectInput(session, "location", choices = unique(joined_data$name))
  
  # Render wind speed histogram based on selected location
  output$windHistogram <- renderPlot({
    req(input$location)  # Ensure a location is selected
    
    filtered_data <- joined_data %>%
      filter(name == input$location)
    
    ggplot(filtered_data, aes(x = wind_speed)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      labs(title = paste("Wind Speed Histogram for", input$location),
           x = "Wind Speed (mph)", y = "Frequency") +
      theme_minimal()
  })
  
#-----------------------
  
  
  output$timeSeriesPlot <- renderPlot({
    req(input$location)  # Ensure a location is selected
    
    ggplot(
      joined_data %>%
        filter(name == input$location) %>%
        mutate(current_temperature = as.numeric(current_temperature)),
      aes(x = time_of_search, y = current_temperature)
    ) +
      geom_line() +
      labs(
        title = paste("Current Temperature Time Series for", input$location),
        x = "Time",
        y = "Current Temperature (°C)"
      ) +
      theme_minimal()
  })
  
  
  
  
  
  
  #----------------
  # Function to get data summaries
  getDataSummary <- function(con) {
    tables <- c("Locations", "PollenLevels", "PollutionLevels", "UVIndexLevels", 
                "VisibilityLevels", "WeatherConditions", "WeatherReports", 
                "WindDirections", "sqlite_sequence")
    
    summaries <- lapply(tables, function(tbl) {
      df <- dbReadTable(con, tbl)
      summary_stats <- summary(df)
      list(
        table = tbl,
        summary = summary_stats
      )
    })
    
    return(summaries)
  }
  
  # Render dataset summary
  output$data_summary <- renderDT({
    # Get summaries from database
    summaries <- getDataSummary(con)
    
    # Convert summaries to a data frame
    summary_df <- do.call(rbind, lapply(summaries, function(x) {
      # Convert the summary to a string for display
      summary_str <- capture.output(print(x$summary))
      data.frame(
        Table = x$table,
        Summary = I(list(paste(summary_str, collapse = "\n")))
      )
    }))
    
    # Render the data table
    datatable(summary_df, options = list(pageLength = 10))
  })
  
  
  
  #------------------
  
  
  # Render Foreign Key Relationships
  output$fk_relationships <- renderDT({
    # Get foreign key relationships for each table
    tables <- dbListTables(con)
    fk_relationships <- lapply(tables, function(table) {
      dbGetQuery(con, paste("PRAGMA foreign_key_list(", table, ");", sep = ""))
    })
    names(fk_relationships) <- tables
    
    # Create a data frame to store relationships
    relationship_data <- do.call(rbind, lapply(names(fk_relationships), function(table) {
      df <- fk_relationships[[table]]
      if (nrow(df) > 0) {
        data.frame(
          Table = table,
          FK_Column = df$from,
          Referenced_Table = df$table,
          Referenced_Column = df$to,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    }))
    
    # Check if the data frame is empty
    if (nrow(relationship_data) == 0) {
      # Return a message if there are no foreign key relationships
      return(datatable(data.frame(Message = "No foreign key relationships found"), 
                       options = list(pageLength = 1, autoWidth = TRUE)))
    }
    
    # Render the data table
    datatable(relationship_data, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  

  
  #--------------
  
  # Ensure that 'weather_reports_data' is correctly assigned
  weather_reports_data <- dbGetQuery(con, "SELECT * FROM WeatherReports")
  
  # Clean the data if needed
  weather_reports_data <- cleanWeatherData(weather_reports_data)
  
  # Render the information box with accurate count
  output$variable_count <- renderText({
    # Get column names and exclude 'id'
    all_columns <- names(weather_reports_data)
    relevant_columns <- setdiff(all_columns, "id")
    
    # Print column names for debugging
    print(relevant_columns)
    
    # Return the number of relevant columns
    length(relevant_columns)
  })
  
  
  
  
  #-------------
  
  
  output$time_diff <- renderText({
    start_time <- min(data$time_of_search, na.rm = TRUE)
    end_time <- max(data$time_of_search, na.rm = TRUE)
    time_diff <- difftime(end_time, start_time, units = "days")
    paste(round(time_diff, 2), "days")
  })
  
  # Render the number of unique locations
  output$unique_locations <- renderText({
    length(unique(locations$name))  # Number of unique locations
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
