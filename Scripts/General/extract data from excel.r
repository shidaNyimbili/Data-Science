#load packages
source("scripts/r prep2.r")

#Set your working directory to the folder where your Excel files are located
setwd("your_working_directory_path")

#Create a list of Excel file names that you want to collect data from
file_list <- list.files(pattern = "\\.xlsx$")

#Define the format you want for the consolidated Excel file.
#For example, you can create a data frame with specific column names and data types
# Example format
output_data <- data.frame(
  EmployeeID = character(0),
  FirstName = character(0),
  LastName = character(0),
  Salary = numeric(0),
  StringsAsFactors = FALSE
)


#Loop through the Excel files, read data from each sheet,
#and append it to the output_data data frame:

##This code assumes that each Excelsheet has the same structure (columns) 
#as the format you defined.

for (file in file_list) {
  # Read data from the Excel file
  sheet_data <- read_xlsx(file)
  
  # Append data to the output_data data frame
  output_data <- rbind(output_data, sheet_data)
}

#Finally, write the consolidated data to a new Excel file:
write_xlsx(output_data, "consolidated_data.xlsx")


#===========================================================#
  
  
# Load required libraries
library(readxl)
library(writexl)

# Set the path to the directory containing the Excel files
input_path <- "path/to/excel/files"

# List all Excel files in the directory
excel_files <- list.files(path = input_path, pattern = "*.xlsx", full.names = TRUE)

# Create an empty data frame to store the consolidated data
consolidated_data <- data.frame()

# Loop through each Excel file
for (file in excel_files) {
  # Read data from each sheet in the Excel file
  sheets <- excel_sheets(file)
  for (sheet in sheets) {
    sheet_data <- read_excel(file, sheet = sheet)
    consolidated_data <- rbind(consolidated_data, sheet_data)
  }
}

# Perform any necessary data transformations or formatting on consolidated_data
# ...

# Set the path and filename for the output Excel file
output_file <- "path/to/output/consolidated_data.xlsx"

# Write the consolidated data to a new Excel file
write_xlsx(consolidated_data, output_file)

cat("Consolidation completed. Data written to:", output_file)




