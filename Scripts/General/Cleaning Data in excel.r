# Load required libraries
library(readxl)
library(dplyr)

# Replace 'your_file_path.xlsx' with the actual path to your Excel file
# Read data from Excel file
data <- read_excel("your_file_path.xlsx", sheet = "Sheet1") 

# Task 1: Handling Missing Values
# Drop rows with any missing values
data <- na.omit(data)

# Task 2: Removing Duplicates
data <- distinct(data)

# Task 3: Correcting Data Types (if needed)
# Example: If a column contains numeric values but was read as character, convert it to numeric.
data$numeric_column <- as.numeric(data$numeric_column)

# Task 4: Data Transformation (if needed)
# Example: Convert dates from character to Date type
data$date_column <- as.Date(data$date_column, format = "%Y-%m-%d")

# Task 5: Data Filtering (if needed)
# Example: Remove rows where a specific column meets certain conditions
data <- filter(data, column_name != "value_to_exclude")

# Task 6: Data Aggregation (if needed)
# Example: Group data by a column and calculate summary statistics
aggregated_data <- data %>%
  group_by(grouping_column) %>%
  summarise(mean_value = mean(numeric_column),
            max_value = max(numeric_column))

# Task 7: Exporting Cleaned Data to a New Excel File
# Replace 'cleaned_data_file.xlsx' with the desired name for the new file
write_excel_csv(aggregated_data, "cleaned_data_file.xlsx", col_names = TRUE)

# Optionally, you can write to a new sheet in the same Excel file using `write_excel_csv` function.
# write_excel_csv(aggregated_data, "your_file_path.xlsx", sheet = "Cleaned Data", col_names = TRUE)
