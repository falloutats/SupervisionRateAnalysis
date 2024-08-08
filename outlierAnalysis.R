# Install and load required packages
install.packages("readxl")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("tidyr")
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)

# Load the Excel file
file_path <- "C:\\Users\\sayde.jude\\OneDrive - Currie & Brown\\Desktop\\Current Labor Rate Database (Projects).xlsx"
labor_rate <- read_excel(file_path, sheet = "Labor", range = "A1:AG11208", col_names = TRUE)

# Filter and select the required columns
engineer <- labor_rate %>%
  filter(Position == "Engineer") %>%                                 # Filter column 1 for "Engineer"
  mutate(Level = ifelse(is.na(Level), "General", Level)) %>%         # Assign all NA values under "Level" to "General"
  mutate(Time = ifelse(is.na(Time), "Reg", Time)) %>%                # Assign all NA values under "Time" to "Reg"
  select(Level = 3,                                                  # Select column 3 titled "Level"
         Time = 5,                                                   # Select column 5 titled "Time"
         Year = 17,                                                  # Select column 18 titled "Year"
         `Bill Rate` = 19) %>%                                       # Select column 20 titled "Bill Rate"
  drop_na(Year)                                                      # Drop rows with NA in the "Year" column

# Print the resulting table
print(engineer)

#-------------------Bill ranges of uncleaned data

# Calculate the range of Bill Rates for each unique combination
bill_rate_ranges <- engineer%>%
  group_by(Level, Time, Year) %>%  # Group by Level, Time, and Year
  summarize(
    Min_Bill_Rate = min(`Bill Rate`, na.rm = TRUE),
    Max_Bill_Rate = max(`Bill Rate`, na.rm = TRUE),
    .groups = 'drop'  # Prevents grouping in the resulting data frame
  )

# Print the range of Bill Rates
print(n=114, bill_rate_ranges)

#-----------------------Looking at side by side box plots of engineers in 2024
# Create side-by-side box plots in 2024
ggplot(subset(engineer, Year == 2024), aes(x = interaction(Level, Time), y = `Bill Rate`)) +
  geom_boxplot(aes(fill = Time)) +
  labs(title = "Box Plots of Bill Rates for Engineers in 2024",
       x = "Level and Time",
       y = "Bill Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------Find 5% and 95% quartiles for the neccessary subsets

# Calculate the 5% and 95% quartiles for each unique combination
percentiles <- engineer %>%
  group_by(Level, Time, Year) %>%   # Group by Level, Time, State, and Year
  summarize(
    `5th_Percentile` = quantile(`Bill Rate`, 0.05, na.rm = TRUE),   # 5th percentile
    `95th_Percentile` = quantile(`Bill Rate`, 0.95, na.rm = TRUE)    # 95th percentile
  )

# Print the percentiles table
print(n=200,percentiles)

#--------------------Remove any data points outside of that quartile range

# Join the percentiles back with the original data to filter out outliers
filtered_data <- engineer %>%
  left_join(percentiles, by = c("Level", "Time", "Year")) %>%
  filter(
    `Bill Rate` >= `5th_Percentile` & `Bill Rate` <= `95th_Percentile`
  ) %>%
  select(-`5th_Percentile`, -`95th_Percentile`)  # Drop percentile columns if no longer needed

# Print the filtered data
print(filtered_data)


#-------------------Bill ranges of cleaned data 

# Calculate the range of Bill Rates for each unique combination
bill_rate_ranges <- filtered_data %>%
  group_by(Level, Time, Year) %>%  # Group by Level, Time, and Year
  summarize(
    Min_Bill_Rate = min(`Bill Rate`, na.rm = TRUE),
    Max_Bill_Rate = max(`Bill Rate`, na.rm = TRUE),
    .groups = 'drop'  # Prevents grouping in the resulting data frame
  )

# Print the range of Bill Rates
print(bill_rate_ranges)




