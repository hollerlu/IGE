library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Study Abroad Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("countryInput", "Choose a Country:", choices = NULL, multiple = TRUE, selected = NULL)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Stacked Bar Chart", plotlyOutput("barChart")),
        tabPanel("Bubble Chart", plotlyOutput("bubbleChart")),
        tabPanel("Choropleth Map", plotlyOutput("mapPlot"))  # Placeholder for your map
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Read and preprocess data
  data <- read.csv("/Users/dylanhong/Desktop/CSC-324/CSC-324 IGE Project/OCS Data 3-7.csv")
  data_aggregated <- data %>%
    group_by(Program.Country, Gender) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    ungroup()
  
  # Calculate top countries by total student count
  top_countries <- data_aggregated %>%
    group_by(Program.Country) %>%
    summarise(Total = sum(Count)) %>%
    arrange(desc(Total)) %>%
    slice_head(n = 5) %>%
    pull(Program.Country)
  
  # Update choices for country input and set default to top countries
  observe({
    updateSelectInput(session, "countryInput", choices = unique(data$Program.Country), selected = top_countries)
  })
  
  # Render Stacked Bar Chart
  output$barChart <- renderPlotly({
    req(input$countryInput)  # Require input to render plot
    df <- data_aggregated %>%
      filter(Program.Country %in% input$countryInput)
    
    p <- ggplot(df, aes(x = Program.Country, y = Count, fill = Gender)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Gender Distribution by Country", x = "Country", y = "Number of Students")
    
    ggplotly(p)  # Make it interactive
  })
  
  # Render Bubble Chart
  output$bubbleChart <- renderPlotly({
    req(input$countryInput)
    df <- data_aggregated %>%
      filter(Program.Country %in% input$countryInput)
    
    p <- ggplot(df, aes(x = Program.Country, y = Gender, size = Count, color = Gender)) +
      geom_point(alpha = 0.6) +
      scale_size(range = c(3, 12)) +
      labs(title = "Bubble Chart of Student Distribution", x = "Country", y = "Gender")
    
    ggplotly(p)
  })
  
  # Render Choropleth Map
  output$mapPlot <- renderPlotly({
    # Placeholder: You'll need to create a choropleth map plot here
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
