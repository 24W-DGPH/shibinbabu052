#  
pacman::p_load(
  rio,  # data import
  here, # relative file pathway
  janitor, # cleans data
  lubridate, # working with dates
  epikit, # age_categories() function
  matchmaker, # dictionary based cleaning
  group_by, # groups one or more variables
  tidyverse, # data management and visualizations
  styler, # source code formatting
  lintr, # detects bad code pattern
  skimr, # preview tibbles
  ggplot2, # data visualization
  zoo, # extra date functions
  as.Date, #date manipulation
  as.POSIXct #date manipulation
)
# import ------------------
 heart_final <- import("heart.csv")
 
 # clean data------
 
 heart <- heart_final %>% 
   
   janitor::clean_names() %>% 
   rename(
     cholesterol = chol
   )
 
 heart = relocate(heart, cp, .after = ca)
 
 heart = relocate(heart, restecg, .after = exang)
 
 heart %>% 
   select(where(is.numeric)) %>%           
   names()
 
# Data Visualization ----------
 
 is.na(heart)  
 
 sum(is.na(heart))
 
 mean(heart$age, na.rm = TRUE)
 
 mean(heart$cholesterol, na.rm = TRUE) 
 
 ggplot(data = heart, mapping = aes(x =age, y = cholesterol))+
   geom_point(color =  "blue") 
 
 ggplot(data = heart, mapping = aes(x = age))+
   geom_histogram(
     binwidth = 7,
     color = "purple",
     fill = "orange"
   ) 
 
 ggplot(data = heart, mapping = aes(x = thalach)) +
   geom_bar(mapping = aes(fill = trestbps), color = "lightblue", na.rm = TRUE)
 
 ggplot(data = heart, 
        mapping = aes(  
          x =age,         
          y = cholesterol,         
          color = thalach,  
          size = thalach))+ 
   geom_point(                      
     alpha = 0.5)
 
 ggplot(data = heart, mapping = aes(x = age))+
   geom_histogram(
     binwidth = 5,
     color = "red",
     fill = "blue"
   )
 
 age_count_plot %>% plotly::ggplotly()
 
 restecg_plot <- 
   ggplot(data = heart, 
          mapping = aes(     #map aesthetics to column values
            x =thalach,         #map x axis to age
            y = exanch,         #map y axis to hours_per_day
            color = age,  #map color to music effects
            size = 0.4))+   
   geom_point(                      #display data as points
     alpha = 0.5)
 music_effect_plot %>% plotly::ggplotly()
 
 
 
 
 
 
 
 