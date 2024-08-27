# Install and load required packages
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(ggplot2)

# Load the Excel file
file_path <- "C:\\Users\\ruby.nganho\\OneDrive - Currie & Brown\\Desktop\\Current Labor Rate Database (Projects).xlsx"
labor_rate <- read_excel(file_path, sheet = "Labor", range = "A1:AG11208", col_names = TRUE)
str(labor_rate)

# Filter and select the required columns for Project Manager
engineer <- labor_rate %>%
  filter(Position == "Engineer") %>%                            # Filter for "Project Manager" only
  mutate(Level = ifelse(is.na(Level), "General", Level)) %>%          # Assign all NA values under "Level" to "General"
  mutate(Time = ifelse(is.na(Time), "Reg", Time)) %>%                 # Assign all NA values under "Time" to "Reg"
  select(Level = 3,                                                   # Select column 3 titled "Level"
         Time = 5,                                                    # Select column 5 titled "Time"
         Year = 16,                                                   # Select column 18 titled "Year"
         `Bill Rate` = 18)                                           # Select column 20 titled "Bill Rate"

engineer$Year <- format(as.Date(engineer$Year, format="%d/%m/%Y"),"%Y")

# Calculate percentiles for groups by Level, Time, and Year
percentiles_engineer <- engineer %>%
  group_by(Level, Time, Year) %>%                                     # Group by Level, Time, and Year
  summarize(
    Count = n(),
    Minimum = if_else(Count == 1, min(`Bill Rate`, na.rm = TRUE), quantile(`Bill Rate`, 0.05, na.rm = TRUE)),
    Maximum = if_else(Count == 1, max(`Bill Rate`, na.rm = TRUE), quantile(`Bill Rate`, 0.95, na.rm = TRUE)),
    .groups = 'drop'
  )

# Define custom levels for Level and Time
custom_levels <- c("General","Senior","Lead","Assistant","Manager","Principal","Intern","Chief","Director","Mid","Junior")
custom_times <- c("Reg", "OT", "2T")

# Add the percentile ranges to the original data
engineer_with_percentiles <- engineer %>%
  left_join(percentiles, by = c("Level", "Time", "Year")) %>%         # Join percentiles by Level, Time, and Year
  select(Level, Time, Year, Minimum, Maximum) %>%                      # Select relevant columns
  distinct() %>%                                                        # Keep only unique rows
  mutate(Level = factor(Level, levels = custom_levels),                # Set custom order for Level
         Time = factor(Time, levels = custom_times)) %>%                # Set custom order for Time
  arrange(Year, Level, Time)                                            # Arrange by Year, then Level, then Time

# Print the resulting data with the percentile range
print(n = 100, engineer_with_percentiles)
