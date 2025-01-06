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