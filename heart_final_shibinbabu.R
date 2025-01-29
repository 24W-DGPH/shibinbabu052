#  
pacman::p_load(
  rio, 
  here, 
  janitor, 
  lubridate,
  epikit, 
  matchmaker, 
  group_by, 
  tidyverse, 
  styler, 
  lintr, 
  skimr, 
  ggplot2, 
  zoo, 
  as.Date, 
  as.POSIXct 
)

# import ------------------
 heart_final <- import("heart.csv")
 heart <- heart_final %>% 
 
 # clean data------
 
 heart <- heart_final %>%
   
  janitor::clean_names() %>%    #cleaning data
   rename(                     #rename the headings
     Cholesterol = chol,
     The_Rest_Blood_Pressure = trestbps,
     Maximum_pulse_acheived = thalach,
     Exercise_induced_Angina = exang,
     Chest_pain_type = cp,
     Fasting_blood_sugar = fbs,
     Major_vessels = ca,
     Resting_ecg = restecg
   )
 
 heart = relocate(heart, Chest_pain_type, .after = Major_vessels) #relocate Chest_pain_type
 
 heart = relocate(heart, Resting_ecg, .after = Exercise_induced_Angina) #relocate Resting_ecg
 
 
 heart %>% 
   select(where(is.numeric)) %>%   #check the columns which are class numeric    
   names()
 

   heart %>% 
   select(contains("age")) %>% 
   head()  # Show the first few rows of the selected column
 
 
   
 
# Data Visualization ----------
 
 
 is.na(heart)  #to find the missing values, if any
 
 sum(is.na(heart)) #to find the total number of missing values, if any
 
 mean(heart$Cholesterol, na.rm = TRUE) #to find the mean value of cholesterol
 
 mean(heart$age, na.rm = TRUE) #to find the mean value of age
 
 ggplot(data = heart, mapping = aes(x =age, y = Cholesterol))+
   geom_point(color =  "blue") 
 
 ggplot(data = heart, mapping = aes(x = age))+
   geom_histogram(
     binwidth = 7,
     color = "purple",
     fill = "orange"
   ) 
 
 ggplot(data = heart, aes(x = The_Rest_Blood_Pressure)) +
   geom_histogram(binwidth = 5, fill = "lightblue", color = "black", na.rm = TRUE) +
   labs(title = "Distribution of Resting Blood Pressure", x = "Resting Blood Pressure", y = "Count") +
   theme_minimal()
 
 
 ggplot(data = heart, 
        mapping = aes(  
          x =age,     #map x axis to age    
          y = Cholesterol,   #map y axis to Cholesterol      
          colour =  The_Rest_Blood_Pressure,  
          size = The_Rest_Blood_Pressure))+ 
   geom_point(                      
     alpha = 0.5)
 
 ggplot(data = heart, mapping = aes(x = age))+
   geom_histogram(
     binwidth = 5,
     color = "pink",
     fill = "black"
   )
 age_count_plot %>% plotly::ggplotly()
 
 heart_plot <- 
   ggplot(data = heart, 
          mapping = aes(     
            x = age,         
            y = Cholesterol,         
            color = age,  
            size = 0.4))+   
   geom_point(                     
     alpha = 0.5)
 heart_plot %>% plotly::ggplotly()
 
 ggplot(data = heart, aes(x = age)) +            #Histogram for age distribution 
   geom_histogram(binwidth = 5, fill = "orange", color = "black") +
   labs(title = "Age Distribution with Bin Width of 5")
 
 
 ggplot(data = heart, aes(x = factor(age), y = Maximum_pulse_acheived, fill = factor(sex))) +
   geom_bar(stat = "identity") +
   labs(title = "Age and the Maximum Pulse Achieved",    #Bar plot for Age vs. Maximum_pulse_achieved
        x = "Age",
        y = "Maximum Pulse Achieved") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 ggplot(data = heart, aes(x = `age`, y = The_Rest_Blood_Pressure, color = `Chest_pain_type`)) +
   geom_point(alpha = 0.5) +
   labs(title = "Plot between age and The_Rest_Blood_Pressure",
        x = "age",
        y = "The_Rest_Blood_Pressure") +
   theme_minimal()
 


 
 
 
 
 
 
 
 
 