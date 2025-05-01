source("Scripts/Data Science/r prep2.r")


# Log file path
log_file <- "etl_process_log.txt"

# Function to log messages to a file
log_message <- function(message) {
  timestamp <- Sys.time()  # Get the current timestamp
  log_entry <- paste(timestamp, "-", message)  # Format the log entry
  write(log_entry, file = log_file, append = TRUE)  # Append to the log file
}

# Log the start of the ETL process
log_message("ETL process started.")

#'*ETL + Data Load into PostgreSQL pipeline*

# Connect to PostgreSQL and create a new database
log_message("Attempting to connect to the PostgreSQL database.")

con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'postgres',
                 host = 'localhost',      # or IP address
                 port = 5432,
                 user = 'postgres',       # or your username
                 password = 'Sh1d@804011')

# Check if the connection is ok
if (dbIsValid(con)) {
  print("Connection successful!")
} else {
  print("Connection failed!")
}

# # Create the new database
# dbExecute(con, "CREATE DATABASE health_data")


# Check if the database exists
db_check_query <- "SELECT 1 FROM pg_database WHERE datname = 'health_data'"

# Query the database to check if 'health_data' exists
db_check_result <- dbGetQuery(con, db_check_query)

# If the database doesn't exist, create it
if (nrow(db_check_result) == 0) {
  dbExecute(con, "CREATE DATABASE health_data")
  print("Database created successfully!")
} else {
  print("Database 'health_data' already exists. Skipping creation.")
}


# Now connect to the newly created database
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'health_data',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'Sh1d@804011')

log_message("Connected to the 'health_data' database.")

# Check if the connection is ok
if (dbIsValid(con)) {
  print("Connection successful!")
} else {
  print("Connection failed!")
}


# # check the database version
# result <- dbGetQuery(con, "SELECT version();")
# print(result)

df <- read.xlsx("Data/fy24q4.xlsx")


#Pushing the data in df to PostgreSQL
dbWriteTable(con, "fy24q4", df, overwrite = TRUE, row.names = FALSE) #This is for new push of data
#dbWriteTable(con, "fy24q4", df, append = TRUE, row.names = FALSE) #This is for appending data


# Chek that data has been pushed successfully by querying the first few rows
query_check <- dbGetQuery(con, "SELECT COUNT(*) FROM fy24q4")

# If the count is greater than 0, the data is written
if (query_check$count > 0) {
  print("Data has been successfully written to the table 'fy24q4'.")
  #send_email("ETL Process Success", "The data has been successfully written to the table 'fy24q4' in the health_data database.")
} else {
  print("Data write failed. No records found in 'fy24q4' table.")
  #send_email("ETL Process Failed", "The data write to the table 'fy24q4' in the health_data database failed.")
}


# Check if data has been pushed and view 10 rows
view_data <- dbGetQuery(con, "SELECT * FROM fy24q4 LIMIT 10")

print(view_data)



#Extracting/querying the data into excel for sharing
query_result <- dbGetQuery(con, "SELECT * FROM fy24q4")

glimpse(query_result)

current_date <- Sys.Date()

# Save with generated date
output_file_name <- paste0("Data/extracted_data_fy24q4_", current_date, ".xlsx")

write_xlsx(query_result, output_file_name)

# Export full table to Excel
query_result <- dbGetQuery(con, "SELECT * FROM fy24q4")
output_file_name <- paste0("processed_files/extracted_healthdata_", Sys.Date(), ".xlsx")
write_xlsx(query_result, output_file_name)
log_message(paste("Exported full dataset to:", output_file_name))




# Close database connection
log_message("Closing the database connection.")
dbDisconnect(con)
log_message("Database connection closed.")

# Final log entry
log_message("ETL process completed.")


