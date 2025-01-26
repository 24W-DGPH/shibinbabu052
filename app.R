#renv --------------------

# Load necessary packages
pacman::p_load(
  rio,  # data import
  here, # relative file pathway
  janitor, # cleans data
  lubridate, # working with dates
  epikit, # age_categories() function
  matchmaker, # dictionary-based cleaning
  dplyr, # data management
  ggplot2, # data visualization1
  shiny   # shiny app
)


# Assuming 'heart_final_shibin' dataset is loaded previously in the code
source("heart_final_shibin.R")


# Load necessary libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)


# Define UI
ui <- fluidPage(
  titlePanel("Heart Panel"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", label = "Age", min = 0, max = 100, value = c(0, 100)),
      sliderInput("cholesterol", label = "Cholesterol", min = 0, max = 500, value = c(0, 500))
    ),
    mainPanel(
      plotOutput("scatter1"),     # First static plot
      plotlyOutput("interactive") # Interactive plot using ggplotly
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Function to create age categories using `cut()`
  create_age_categories <- function(age) {
    cut(age, 
        breaks = c(0, 18, 35, 50, 65, 100), 
        labels = c("0-18", "19-35", "36-50", "51-65", "65+"), 
        include.lowest = TRUE)
  }
  
  # Function to generate static scatter plot
  scatter1 <- function(heart, age_filter, cholesterol_filter) {
    filtered_data <- heart %>% 
      filter(age >= age_filter[1] & age <= age_filter[2],
             cholesterol >= cholesterol_filter[1] & cholesterol <= cholesterol_filter[2])
    
    ggplot(data = filtered_data, 
           mapping = aes(x = age, y = cholesterol, color = age, size = cholesterol)) +
      geom_point(alpha = 0.3) +
      labs(title = "Age vs Cholesterol", x = "Age", y = "Cholesterol") +
      theme_minimal()
  }
  
  # Reactive function for filtered scatter plot
  scatter_filter1 <- reactive({
    scatter1(heart, input$age, input$cholesterol)
  })
  
  # Render static scatter plot
  output$scatter1 <- renderPlot({
    scatter_filter1()
  })
  
  # Render interactive plot using ggplotly
  output$interactive <- renderPlotly({
    filtered_data <- heart %>% 
      filter(age >= input$age[1] & age <= input$age[2],
             cholesterol >= input$cholesterol[1] & cholesterol <= input$cholesterol[2]) %>%
      mutate(age_group = create_age_categories(age)) # Add age categories
    
    heart_plot <- ggplot(data = filtered_data, 
                         mapping = aes(
                           x = age, 
                           y = cholesterol, 
                           color = age_group)) +
      geom_point(alpha = 0.7) +
      labs(title = "Interactive Heart Plot", 
           x = "Age", 
           y = "Cholesterol", 
           color = "Age Group") +
      theme_minimal()
    
    ggplotly(heart_plot)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
