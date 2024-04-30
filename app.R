
# libraries
library(magrittr)
library(tidyverse)
library(ggplot2) 
library(tidyr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)
library(RColorBrewer)
suppressWarnings(suppressMessages(library(magrittr)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(tidyr)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(shiny)))
suppressWarnings(suppressMessages(library(shinydashboard)))
suppressWarnings(suppressMessages(library(plotly)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(leaflet)))
suppressWarnings(suppressMessages(library(maps)))
suppressWarnings(suppressMessages(library(RColorBrewer)))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(plotly))

# DATA CLEANING/MANIPULATION
# main data file
dataFile <- read.csv("https://raw.githubusercontent.com/hollerlu/IGE/main/Updated%20OCS%20Data%203-7.csv")
dataFile$ProgramCity <- gsub(",.*", "", dataFile$ProgramCity)

cityMap <- read.csv("https://raw.githubusercontent.com/naramirezj/Off_Campus_Study_R/main/worldcities.csv")

activePrograms <- read.csv("https://raw.githubusercontent.com/hollerlu/IGE/main/OCS%2024-25%20Active%20Programs.csv", header = TRUE, stringsAsFactors = FALSE)

# data for finance visualization
finData <- read.csv("https://raw.githubusercontent.com/hollerlu/IGE/main/OCS%20Program%20Cost%20Comparison.csv")

# create dataframe specifically for term visualization
# subset program year and term
ocs_term2 <- subset(dataFile, select = c(AcademicYear,ProgramTerm))

# calculate student totals according to year/term criteria
ocs_term2_summary <- ocs_term2 %>%
  group_by(AcademicYear,ProgramTerm) %>%
  summarise(total_students = n(), .groups = "drop")
# head(ocs_term2_summary)

# create a dataframe of countries and the number of students who have visited them
countryCounts <- dataFile %>%
  group_by(ProgramRegion) %>%
  summarise(TripCount = n(), ProgramNames = list(ProgramName), .groups = "drop")

d <- dataFile %>%
  group_by(ProgramRegion, ProgramYear) %>%
  summarize(TripCount = n(), .groups = "drop") %>%
  select(ProgramRegion, TripCount, ProgramYear)

# create dataframe for gender visualization
data_aggregated <- dataFile %>%
  group_by(CountryName, Gender) %>%
  summarise(Count = n(), .groups = "drop") %>%
  ungroup()

# calculate top countries by total student count
top_countries <- data_aggregated %>%
  group_by(CountryName) %>%
  summarise(Total = sum(Count), .groups = "drop") %>%
  arrange(desc(Total)) %>%
  slice_head(n = 5) %>%
  pull(CountryName)

# data cleaning for financial visualization
finData <- finData %>% rename(ProgramName = Program.Name)
finData <- finData %>% rename(CostComparisonToGrinnell = Cost.Comparison.to.Grinnell..see.Key.tab.)
finData <- finData %>% rename(FinAidAvailable = Estimated.OCS.Grant.for.Need.Based.Aid.Recipients..per.semester.)
finData <- subset(finData, select = -c(X))
finData <- finData[-c(91:94), ]

finData <- finData %>%
  group_by(ProgramName) %>%
  filter(!(duplicated(ProgramName) & Term == "Academic Year")) %>%
  ungroup()


# cost comparison variable for finance data
get_cost_value <- function(value) {
  if (value == 1) {
    return("Less expensive")
  } else if (value == 2) {
    return("Equal cost")
  } else if (value == 3) {
    return("Likely $0-$1250 more")
  } else if (value == 4) {
    return("Likely $1250-$2500 more")
  } else if (value == 5) {
    return("Likely $2500 more")
  } else {
    return("Unknown")  # Handle any unexpected values
  }
}
finData$CostValueComparison <- sapply(finData$CostComparisonToGrinnell, get_cost_value)

# SHINY APP
# UI
ui <- dashboardPage(
  skin = "black",
  # title
  dashboardHeader(title = "OCS/IGE"),
  
  # side bar options
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map Vis", tabName = "map"),
      menuItem("Ethnicity Vis", tabName = "ethnicity"),
      menuItem("Term Vis", tabName = "term"),
      menuItem("Trip Count Vis", tabName = "linechart"),
      menuItem("Gender Vis", tabName = "gender"),
      menuItem("Financial Vis", tabName = "finance")
    )
  ),
  
  # body
  dashboardBody(
    tabItems(
      # map tab
      tabItem(
        tabName = "map",
        h2("Off-Campus Programs Map"),
        column(width = 12,
               leafletOutput("map")),
        checkboxGroupInput("selectedRegion", "Choose Region:", choices = unique(dataFile$ProgramRegion), selected=unique(dataFile$ProgramRegion))),
      # ethnicity bar chart tab
      tabItem(
        tabName = "ethnicity",
        h2("Ethnicity Per Region"),
        fluidRow(
          box(
            # select region
            checkboxGroupInput("region", "Select Region(s):",
                               choices = unique(dataFile$ProgramRegion),
                               # exclude europe as default
                               selected = unique(dataFile$ProgramRegion[dataFile$ProgramRegion != "Europe"])))),
        fluidRow(
          box(
            plotlyOutput("plot_ethnicity")))),
      # students abroad per term bar chart
      tabItem(tabName = "term",
              h2("Term Abroad Per Year"),
              fluidRow(
                box(
                  plotlyOutput("plot_term")))),
      # trip count line chart
      tabItem(tabName = "linechart",
              h2("Trip Count per Year"),
              fluidRow(
                box(
                  # select region
                  checkboxGroupInput("line_region", "Select Region(s):",
                                     choices = unique(d$ProgramRegion),
                                     # exclude europe as default
                                     selected = unique(d$ProgramRegion[d$ProgramRegion != "Europe"])))),
              fluidRow(
                box(
                  plotOutput("lineplot")))),
      # gender visualizations
      tabItem(tabName = "gender",
              h2("Gender Distribution"),
              fluidRow(
                box(
                  # select country
                  selectInput("countryInput", "Choose a Country:", choices = NULL, multiple = TRUE, selected = NULL))),
              # bar chart vis
              fluidRow(
                box(
                  plotlyOutput("barChart"))),
              # normalized bar chart vis
              fluidRow(
                box(
                  plotlyOutput("normalizedBarChart"))),
              # bubble chart vis
              fluidRow(
                box(
                  plotlyOutput("bubbleChart")))),
      # finance visualization
      tabItem(tabName = "finance",
              h2("Cost Comparison between a semester at Grinnell vs. Study Abroad Programs"),
              fluidRow(
                box(
                  plotlyOutput("costComparisonPlot"))
              ))
    )
  )
)


# SERVER
server <- function(input, output, session) {
  # MAP
  # Reading in the country information
  
  cityData <- reactive({
    req(input$selectedRegion)
    
    regionFilteredData <- dataFile %>%
      filter(ProgramRegion %in% input$selectedRegion) %>%
      filter(ProgramName %in% activePrograms$Program.Name) %>%
      group_by(ProgramCity) %>%
      mutate(TripCount = 0) 
    
    cityData_merge <- merge(regionFilteredData, cityMap[, c("city_ascii", "country", "lat", "lng")], 
                            by.x = c("ProgramCity", "CountryName"), 
                            by.y = c("city_ascii", "country"), all.x = TRUE)  
    
    cityDataGrouped <- cityData_merge %>%
      group_by(ProgramCity) %>%
      summarise(lat = first(lat),
                lng = first(lng),
                TripCount = n_distinct(ApplicationID), .groups = "drop")
    return(cityDataGrouped)
  })
  
  #map
  # Creating the map
  output$map <- renderLeaflet({
    # When a city is clicked on, makes a popup box that shows its name,
    # how many visits it's had, and links to its active programs.
    popupText <- lapply(seq(nrow(cityData())), function(i) {
      city <- cityData()$ProgramCity[i]
      unique_programs <- unique(dataFile$ProgramName[dataFile$ProgramCity == city & dataFile$ProgramRegion %in% input$selectedRegion])
      active_unique_programs <- unique_programs[unique_programs %in% activePrograms$Program.Name]
      program_links <- paste("<a href='", activePrograms$TD.link[activePrograms$Program.Name %in% unique_programs], "'>", active_unique_programs, "</a>", collapse = "<br/>")
      paste(
        "City: ", city, "<br/>",
        "Visits: ", cityData()$TripCount[cityData()$ProgramCity == city], "<br/>",
        "Programs: ", paste(program_links, collapse = "<br/>"),
        sep = ""
      )
    }) %>% 
      lapply(htmltools::HTML)
    
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      # Sets the limits of scrolling
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      addMarkers(data = cityData(), 
                 lng = ~lng, 
                 lat = ~lat, 
                 popup = ~popupText,
                 clusterOptions = markerClusterOptions(spiderLegPolylineOptions = list(weight = 0.5, color = "#222", opacity = 0.5)))
  })
  
  # ETHNICITY VIS
  output$plot_ethnicity <- renderPlotly({
    ocs_filter <- dataFile %>%
      # filter out missing ethnicity data
      filter(Ethnicity != "") %>%
      # user can select regions
      filter(ProgramRegion %in% input$region)
    
    
    plot_ly(ocs_filter,
            x = ~CountryName, 
            color = ~Ethnicity,
            type = "histogram"
            # hoverinfo = "text",
            #hovertext = paste("Number of students: ",ocs_filter$NumStudents)
    ) %>%
      # stacked bar chart
      layout(barmode = "stack",
             xaxis = list(title = "Country Name"),
             yaxis = list(title = "Number of Students Abroad"))
    
  })
  
  
  # TERM VIS
  output$plot_term <- renderPlotly({
    # order x axis chronologically
    x_order <- list(categoryorder = "array",
                    categoryarray = c("2016-2017", "2017-2018", "2018-2019", "2019-2020", "2020-2021", "2021-2022", "2022-2023"))
    
    # stacked bar chart
    plot_ly(ocs_term2_summary, 
            x = ~AcademicYear, 
            y = ~total_students, 
            color = ~ProgramTerm, 
            type = "bar",
            text = ~ProgramTerm,
            hoverinfo = "text",
            hovertext = paste("Number of students abroad over", ocs_term2_summary$ProgramTerm,
                              ": ", ocs_term2_summary$total_students)) %>%
      layout(barmode = "stack",
             xaxis = list(x_order,
                          title = "Academic Year"),
             yaxis = list(title = "Total Students Abroad Each Year"))
  })
  
  # TRIP COUNT VIS
  output$lineplot <- renderPlot({
    
    # filter data based on selected regions
    filtered_data <- d %>% filter(ProgramRegion %in% input$line_region)
    
    # create line graph
    ggplot(data = filtered_data, aes(x = ProgramYear, y = TripCount, color = ProgramRegion)) +
      geom_line(size = 1) +
      labs(x = "Year", y = "Trip Count", title = "Trip Count by Region Over Time") +
      theme_minimal()
  })
  
  # GENDER VIS
  observe({
    updateSelectInput(session, "countryInput", choices = unique(data_aggregated$CountryName), selected = top_countries)
  })
  
  # render Stacked Bar Chart
  output$barChart <- renderPlotly({
    # require input to render plot
    req(input$countryInput)  
    df <- data_aggregated %>%
      filter(CountryName %in% input$countryInput)
    
    p <- ggplot(df, aes(x = CountryName, y = Count, fill = Gender)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("F" = "#AD62AA", "M" = "#659DBD")) +
      labs(title = "Gender Distribution by Country", x = "Country", y = "Number of Students")
    
    # make interactive
    ggplotly(p) 
  })
  
  # render relative height stacked bar chart
  output$normalizedBarChart <- renderPlotly({
    req(input$countryInput)
    df <- data_aggregated %>%
      filter(CountryName %in% input$countryInput) %>%
      group_by(CountryName) %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    p <- ggplot(df, aes(x = CountryName, y = Percentage, fill = Gender)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_manual(values = c("F" = "#AD62AA", "M" = "#659DBD")) +
      labs(title = "Relative Gender Distribution by Country", x = "Country", y = "Percentage")
    
    ggplotly(p) 
  })
  
  # render bubble chart
  output$bubbleChart <- renderPlotly({
    req(input$countryInput)
    df <- data_aggregated %>%
      filter(CountryName %in% input$countryInput)
    
    p <- ggplot(df, aes(x = CountryName, y = Gender, size = Count, color = Gender)) +
      geom_point(alpha = 0.6) +
      scale_color_manual(values = c("F" = "#AD62AA", "M" = "#659DBD")) +
      scale_size(range = c(3, 12)) +
      labs(title = "Bubble Chart of Student Distribution", x = "Country", y = "Gender")
    
    ggplotly(p)
  })
  
  # FINANCE
  # Specify the desired order of CostValueComparison levels
  desired_order <- c("Less expensive", "Equal cost", "Likely $0-$1250 more", "Likely $1250-$2500 more", "Likely $2500 more")
  
  # Convert CostValueComparison to factor with specified levels and order
  finData$CostValueComparison <- factor(finData$CostValueComparison, levels = desired_order)
  
  # Filter data and aggregate by CostValueComparison and ProgramName
  filtered_data <- finData %>%
    filter(!is.na(CostValueComparison) & !is.na(ProgramName)) %>%
    group_by(CostValueComparison, ProgramName, FinAidAvailable) %>%
    summarise(count = n(), .groups = "drop") %>%
    ungroup()
  
  # Define a color palette (adjust as needed)
  program_colors <- scales::hue_pal()(length(unique(filtered_data$ProgramName)))
  
  # Create the plotly plot with custom colors and tooltip
  output$costComparisonPlot <- renderPlotly({
    p <- ggplot(filtered_data, aes(x = CostValueComparison, y = count, fill = ProgramName,
                                   text = paste("Program Name: ", ProgramName, "\n",
                                                "Need-based Aid (if eligible): ", FinAidAvailable))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = program_colors) +  # Assign custom colors based on ProgramName
      labs(x = "Difference between Grinnell and Study Abroad Program", y = "No. of Study Abroad Programs") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Hide legend
    
    ggplotly(p, tooltip = "text", dynamicTicks = TRUE)
  })
  
}

shinyApp(ui, server, options = list(width = 1000, height = 6000))