library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Load the beer data
breweries <- read.csv("/Users/coolp/Desktop/Learning/SMU/Fall 2023/DS 6306/group project 1/Breweries.csv")
beers <- read.csv("/Users/coolp/Desktop/Learning/SMU/Fall 2023/DS 6306/group project 1/Beers.csv")

breweries<-data.frame(breweries)

beers<-data.frame(beers)
colnames(beers)[5]="Brew_ID"

beer_data<- merge(beers,breweries, by="Brew_ID")

# Define the UI
ui <- fluidPage(
  titlePanel("Beer Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("plot_type", "Choose Plot Type",
                   choices = c("Histogram", "Boxplot")),
      selectInput("state_filter", "Filter by State", choices = c("All", unique(beer_data$State))),
      checkboxInput("regression_line", "Add Regression Line", value = FALSE)
    ),
    mainPanel(
      plotOutput("IBU_ABV_plot"),
      plotlyOutput("scatter_plot"),
      plotOutput("additional_plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  data <- reactive({
    req(input$data_file)
    beer_data <- read.csv(input$data_file$datapath)
    return(beer_data)
  })
  
  plot_type_reactive <- reactive({
    if (input$plot_type == "Histogram") {
      return("Histogram")
    } else {
      return("Boxplot")
    }
  })
  
  filtered_data <- reactive({
    data_to_filter <- data()
    if (input$state_filter != "All") {
      data_to_filter <- filter(data_to_filter, State == input$state_filter)
    }
    return(data_to_filter)
  })
  
  output$ibu_abv_plot <- renderPlot({
    plot_type <- plot_type_reactive()
    if (plot_type == "Histogram") {
      ggplot(filtered_data(), aes(x = IBU)) + geom_histogram(binwidth = 10, fill = "blue", color = "black") +
        labs(title = "IBU Histogram", x = "IBU", y = "Frequency")
    } else {
      ggplot(filtered_data(), aes(x = State, y = IBU)) + geom_boxplot(fill = "blue") +
        labs(title = "IBU Boxplot", x = "State", y = "IBU")
    }
  })
  
  output$scatter_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = ABV, y = IBU, color = State)) +
      geom_point() +
      labs(title = "Scatter Plot of ABV vs. IBU", x = "ABV", y = "IBU") +
      theme_minimal()
    
    if (input$regression_line) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    ggplotly(p)
  })
  
  output$additional_plot <- renderPlot({
    # Add your additional plot here.
    # Example: A bar chart of the top 5 breweries with the highest average IBU
    top_breweries <- beer_data %>%
      group_by(brewery_name) %>%
      summarize(avg_IBU = mean(IBU, na.rm = TRUE)) %>%
      top_n(5, wt = avg_IBU)
    
    ggplot(top_breweries, aes(x = reorder(Name.y, -avg_IBU), y = avg_IBU)) +
      geom_bar(stat = "identity", fill = "green") +
      labs(title = "Top 5 Breweries with Highest Average IBU", x = "Brewery Name", y = "Average IBU") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the Shiny app
shinyApp(ui, server)

      