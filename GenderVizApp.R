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
        tabPanel("Relative Height Stacked Bar Chart", plotlyOutput("normalizedBarChart")),
        tabPanel("Bubble Chart", plotlyOutput("bubbleChart")),
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Read and preprocess data
  data <- read.csv("/Users/dylanhong/Desktop/CSC-324/CSC-324 IGE Project/Updated OCS Data 3-7.csv")
  data_aggregated <- data %>%
    group_by(CountryName, Gender) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    ungroup()
  
  # Calculate top countries by total student count
  top_countries <- data_aggregated %>%
    group_by(CountryName) %>%
    summarise(Total = sum(Count)) %>%
    arrange(desc(Total)) %>%
    slice_head(n = 5) %>%
    pull(CountryName)
  
  # Update choices for country input and set default to top countries
  observe({
    updateSelectInput(session, "countryInput", choices = unique(data$CountryName), selected = top_countries)
  })
  
  # Render Stacked Bar Chart
  output$barChart <- renderPlotly({
    req(input$countryInput)  # Require input to render plot
    df <- data_aggregated %>%
      filter(CountryName %in% input$countryInput)
    
    p <- ggplot(df, aes(x = CountryName, y = Count, fill = Gender)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("F" = "#AD62AA", "M" = "#659DBD")) +
      labs(title = "Gender Distribution by Country", x = "Country", y = "Number of Students")
    
    ggplotly(p)  # Make it interactive
  })
  
  # Render Relative Height Stacked Bar Chart
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
    
    ggplotly(p)  # Make it interactive
  })
  
  # Render Bubble Chart
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
}

# Run the application 
shinyApp(ui = ui, server = server)
