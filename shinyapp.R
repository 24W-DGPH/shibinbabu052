# Load necessary packages
pacman::p_load(
  rio,  
  here, 
  janitor,
  lubridate, 
  epikit, 
  matchmaker, 
  dplyr, 
  ggplot2, 
  shiny,
  tidyverse,
  styler,
  lintr,
  skimr,
  plotly
)

# Load dataset with error handling
heart_final_shibinbabu <- tryCatch({
  read.csv("heart.csv")
}, error = function(e) {
  message("File not found! Please select the correct file.")
  read.csv(file.choose())
})

# Clean and rename dataset
heart_final_shibinbabu <- heart_final_shibinbabu %>%
  janitor::clean_names() %>%    
  rename(
    Cholesterol = chol,
    The_Rest_Blood_Pressure = trestbps,
    Maximum_pulse_acheived = thalach,
    Exercise_induced_Angina = exang,
    Chest_pain_type = cp,
    Fasting_blood_sugar = fbs,
    Major_vessels = ca,
    Resting_ecg = restecg
  ) %>%
  relocate(Chest_pain_type, .after = Major_vessels) %>%  
  relocate(Resting_ecg, .after = Exercise_induced_Angina)

# Define UI
ui <- fluidPage(
  titlePanel("Heart Panel"),
  sidebarLayout(
    sidebarPanel(
      # Show Age filter in both tabs
      sliderInput("age", label = "Age", min = 0, max = 100, value = c(0, 100)),
      
      # Show Cholesterol filter only in Tab 1
      conditionalPanel(
        condition = "input.tab_selected === 'tab1'",  
        sliderInput("Cholesterol", label = "Cholesterol", min = 0, max = 500, value = c(0, 500))
      ),
      
      # Show Maximum Pulse filter only in Tab 2
      conditionalPanel(
        condition = "input.tab_selected === 'tab2'",  
        sliderInput("MaxPulse", label = "Maximum Pulse Achieved", min = 50, max = 250, value = c(50, 250))
      ),
      
      # Add action button to manually trigger filtering
      actionButton("apply_filters", "Apply Filters")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tab_selected",  # Added ID to enable correct tab selection
        tabPanel("Age vs Cholesterol", value = "tab1", plotlyOutput("interactive1")),
        tabPanel("Age vs Maximum Pulse Achieved", value = "tab2", plotlyOutput("interactive2"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Function to create age categories
  create_age_categories <- function(age) {
    cut(age, 
        breaks = c(0, 18, 35, 50, 65, 100), 
        labels = c("0-18", "19-35", "36-50", "51-65", "65+"), 
        include.lowest = TRUE)
  }
  
  # Ensure sliders retain values when switching tabs
  observe({
    updateSliderInput(session, "Cholesterol", value = input$Cholesterol)
    updateSliderInput(session, "MaxPulse", value = input$MaxPulse)
  })
  
  # Reactive dataset for Age vs Cholesterol 
  filtered_data1 <- eventReactive(input$apply_filters, {
    heart_final_shibinbabu %>%
      filter(
        age >= (input$age[1] %||% 0) & age <= (input$age[2] %||% 100),  # Fix NULL issue
        Cholesterol >= (input$Cholesterol[1] %||% 0) & 
          Cholesterol <= (input$Cholesterol[2] %||% 500)  # Fix NULL issue
      ) %>%
      mutate(age_group = create_age_categories(age))
  })
  
  # Reactive dataset for Age vs Maximum Pulse Achieved 
  filtered_data2 <- eventReactive(input$apply_filters, {
    heart_final_shibinbabu %>%
      filter(
        age >= (input$age[1] %||% 0) & age <= (input$age[2] %||% 100),  # Fix NULL issue
        Maximum_pulse_acheived >= (input$MaxPulse[1] %||% 50) & 
          Maximum_pulse_acheived <= (input$MaxPulse[2] %||% 250)  # Fix NULL issue
      ) %>%
      mutate(age_group = create_age_categories(age))
  })
  
  # Render interactive plot for Age vs Cholesterol
  output$interactive1 <- renderPlotly({
    req(nrow(filtered_data1()) > 0)  
    heart_plot1 <- ggplot(data = filtered_data1(), 
                          mapping = aes(x = age, y = Cholesterol, color = age_group)) +
      geom_point(alpha = 0.7) +
      labs(title = "Age vs Cholesterol", 
           x = "Age", 
           y = "Cholesterol", 
           color = "Age Group") +
      theme_minimal()
    
    ggplotly(heart_plot1)
  })
  
  # Render interactive plot for Age vs Maximum Pulse Achieved
  output$interactive2 <- renderPlotly({
    req(nrow(filtered_data2()) > 0) 
    heart_plot2 <- ggplot(data = filtered_data2(), 
                          mapping = aes(x = age, y = Maximum_pulse_acheived, color = age_group)) +
      geom_point(alpha = 0.7) +
      labs(title = "Age vs Maximum Pulse Achieved", 
           x = "Age", 
           y = "Maximum Pulse Achieved", 
           color = "Age Group") +
      theme_minimal()
    
    ggplotly(heart_plot2)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)