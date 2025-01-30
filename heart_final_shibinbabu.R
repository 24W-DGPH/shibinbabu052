# Load necessary packages -------
pacman::p_load(
  rio, 
  here, 
  janitor, 
  lubridate,
  epikit, 
  matchmaker, 
  tidyverse, 
  styler, 
  lintr, 
  skimr, 
  ggplot2, 
  zoo,
  plotly
)

# Import dataset ------
heart_final <- import("heart.csv")

# Clean data
heart <- heart_final %>%
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

# Check numeric columns
numeric_cols <- heart %>% 
  select(where(is.numeric)) %>% 
  names()

print(numeric_cols)

# Select and preview 'age' related columns
heart %>% 
  select(contains("age")) %>% 
  head()

# --- Data Exploration ---
# Check for missing values
print(sum(is.na(heart)))  # Total number of missing values
print(mean(heart$Cholesterol, na.rm = TRUE))  # Mean Cholesterol
print(mean(heart$age, na.rm = TRUE))  # Mean Age

# --- Data Visualization ---
# Scatter plot: Age vs Cholesterol
ggplot(data = heart, aes(x = age, y = Cholesterol)) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot: Age vs Cholesterol")

# Histogram: Age Distribution
ggplot(data = heart, aes(x = age)) +
  geom_histogram(binwidth = 7, color = "purple", fill = "orange") +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# Histogram: Resting Blood Pressure Distribution
ggplot(data = heart, aes(x = The_Rest_Blood_Pressure)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black", na.rm = TRUE) +
  labs(title = "Distribution of Resting Blood Pressure", x = "Resting Blood Pressure", y = "Count") +
  theme_minimal()

# Scatter plot with color and size mapped to Blood Pressure
ggplot(data = heart, aes(x = age, y = Cholesterol, color = The_Rest_Blood_Pressure, size = The_Rest_Blood_Pressure)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot: Age vs Cholesterol (Colored by Blood Pressure)")

# Another histogram variation
ggplot(data = heart, aes(x = age)) +
  geom_histogram(binwidth = 5, color = "pink", fill = "black") +
  labs(title = "Age Distribution (Bin Width = 5)")

# Scatter Plot with Plotly
heart_plot <- ggplot(data = heart, aes(x = age, y = Cholesterol, color = age)) +
  geom_point(alpha = 0.5, size = 2) +
  labs(title = "Interactive Scatter Plot: Age vs Cholesterol", x = "Age", y = "Cholesterol")

ggplotly(heart_plot)  # Convert to interactive plot

# Bar Plot: Age vs. Maximum Pulse Achieved
ggplot(data = heart, aes(x = factor(age), y = Maximum_pulse_acheived, fill = factor(sex))) +
  geom_bar(stat = "identity") +
  labs(title = "Age vs Maximum Pulse Achieved", x = "Age", y = "Maximum Pulse Achieved") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Scatter Plot: Age vs Resting Blood Pressure
ggplot(data = heart, aes(x = age, y = The_Rest_Blood_Pressure, color = Chest_pain_type)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot: Age vs Resting Blood Pressure", x = "Age", y = "Resting Blood Pressure") +
  theme_minimal()

