# Install and load required packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(ggplot2)

# Load the Excel file
file_path <- "C:\\Users\\sayde.jude\\OneDrive - Currie & Brown\\Desktop\\Current Labor Rate Database (Projects).xlsx"
labor_rate <- read_excel(file_path, sheet = "Labor", range = "A1:AG11208", col_names = TRUE)
str(labor_rate)

# Filter and select the required columns for Project Manager in 2024
project_manager <- labor_rate %>%
  filter(Position == "Project Manager", Year == 2024) %>%             # Filter for "Project Manager" and "Year" 2024
  mutate(Level = ifelse(is.na(Level), "General", Level)) %>%          # Assign all NA values under "Level" to "General"
  mutate(Time = ifelse(is.na(Time), "Reg", Time)) %>%                 # Assign all NA values under "Time" to "Reg"
  select(Level = 3,                                                   # Select column 3 titled "Level"
         Time = 5,                                                    # Select column 5 titled "Time"
         Year = 17,                                                   # Select column 18 titled "Year"
         `Bill Rate` = 19)                                            # Select column 20 titled "Bill Rate"
drop_na(Year)                                                       # Drop rows with NA in the "Year" column

# Calculate percentiles for groups
percentiles <- project_manager %>%
  group_by(Level, Time) %>%                                           # Group by Level and Time
  summarize(
    Count = n(),
    Minimum = if_else(Count == 1, min(`Bill Rate`, na.rm = TRUE), quantile(`Bill Rate`, 0.05, na.rm = TRUE)),
    Maximum = if_else(Count == 1, max(`Bill Rate`, na.rm = TRUE), quantile(`Bill Rate`, 0.95, na.rm = TRUE)),
    .groups = 'drop'
  )

# Add the percentile ranges to the original data
project_manager_with_percentiles <- project_manager %>%
  left_join(percentiles, by = c("Level", "Time")) %>%                  # Join percentiles by Level and Time
  select(Level, Time, Year, Minimum, Maximum) %>%                      # Select relevant columns without 'Bill Rate'
  distinct()                                                           # Keep only unique rows

# Print the resulting data with the percentile range
print(project_manager_with_percentiles)




