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
str(labor_rate)
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
print(n=500, engineer)

#-------------------Bill ranges of uncleaned data

# Calculate the range of Bill Rates for each unique combination
bill_rate_ranges_before <- engineer%>%
  group_by(Level, Time, Year) %>%  # Group by Level, Time, and Year
  summarize(
    Min_Bill_Rate = min(`Bill Rate`, na.rm = TRUE),
    Max_Bill_Rate = max(`Bill Rate`, na.rm = TRUE),
    .groups = 'drop'  # Prevents grouping in the resulting data frame
  )

# Print the range of Bill Rates
print(n=114, bill_rate_ranges_before)


# Calculate percentiles for groups with more than two data points
percentiles <- engineer %>%
  group_by(Level, Time, Year) %>%
  summarize(
    Count = n(),
    `5th_Percentile` = if_else(Count > 2, quantile(`Bill Rate`, 0.05, na.rm = TRUE), NA_real_),
    `95th_Percentile` = if_else(Count > 2, quantile(`Bill Rate`, 0.95, na.rm = TRUE), NA_real_),
    .groups = 'drop'
  )

# Join the percentiles back with the original data
filtered_data <- engineer %>%
  left_join(percentiles, by = c("Level", "Time", "Year")) %>%             #left_join(percentiles, by = c("Level", "Time", "Year")) merges 
  filter(                                                                 #engineer with percentiles based on matching values in the columns
    # Include all data points for groups with 2 or fewer data points      #Level, Time, and Year. The left_join ensures all rows
    is.na(`5th_Percentile`) |                                             #from engineer are kept, and the matching rows from percentiles are added.
      # For groups with more than 2 data points, filter based on percentiles
      (`Bill Rate` >= `5th_Percentile` & `Bill Rate` <= `95th_Percentile`)
  ) %>%
  select(-`5th_Percentile`, -`95th_Percentile`)                           # Drop percentile columns if no longer needed

# Print the resulting table
print(filtered_data)


#-------------------Bill ranges of cleaned data 

# Calculate the range of Bill Rates for each unique combination
bill_rate_ranges_after <- filtered_data %>%
  group_by(Level, Time, Year) %>%  # Group by Level, Time, and Year
  summarize(
    Min_Bill_Rate = min(`Bill Rate`, na.rm = TRUE),
    Max_Bill_Rate = max(`Bill Rate`, na.rm = TRUE),
    .groups = 'drop'  # Prevents grouping in the resulting data frame
  )

# Print the range of Bill Rates
print(n=200,bill_rate_ranges_after)







