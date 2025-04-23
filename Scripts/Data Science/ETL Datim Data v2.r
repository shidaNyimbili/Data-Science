source("Scripts/Analytics/r prep2.r")

# Log file path
log_file <- "etl_process_log.txt"

log_message <- function(message) {
  timestamp <- Sys.time()
  log_entry <- paste(timestamp, "-", message)
  write(log_entry, file = log_file, append = TRUE)
}

log_message("ETL process started.")

# Connect to default Postgres DB to check/create the health_data DB
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'postgres',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'Sh1d@804011')

# Create database if not present
if (nrow(dbGetQuery(con, "SELECT 1 FROM pg_database WHERE datname = 'health_data'")) == 0) {
  dbExecute(con, "CREATE DATABASE health_data")
  log_message("Database 'health_data' created.")
} else {
  log_message("Database 'health_data' already exists.")
}
dbDisconnect(con)

# Connect to target DB
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'health_data',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'Sh1d@804011')

log_message("Connected to 'health_data' database.")

# Create etl_file_log if it doesn't exist
if (!"etl_file_log" %in% dbListTables(con)) {
  dbExecute(con, "
    CREATE TABLE etl_file_log (
      file_name TEXT PRIMARY KEY,
      loaded_at TIMESTAMP
    );
  ")
  log_message("Created etl_file_log table.")
}

# List all Excel files
data_files <- list.files("Data/processed_files", pattern = "\\.xlsx$", full.names = TRUE)

# Create or connect to the file log table to track loaded files
if (!"etl_file_log" %in% dbListTables(con)) {
  dbExecute(con, "
    CREATE TABLE etl_file_log (
      file_name TEXT PRIMARY KEY,
      loaded_at TIMESTAMP
    );
  ")
}

# Create the main data table if it doesn't exist
if (!"datim_data" %in% dbListTables(con)) {
  # Read the first file to get column structure
  sample_df <- read.xlsx(data_files[1])
  sample_df$source_file <- NA_character_
  sample_df$upload_timestamp <- as.POSIXct(NA)
  
  dbWriteTable(con, "datim_data", sample_df[0, ], overwrite = TRUE, row.names = FALSE)
  log_message("Created unified table: datim_data")
}

# Process each file and append to datim_data
for (file in data_files) {
  file_name <- basename(file)
  
  # Skip if already loaded
  file_already_loaded <- dbGetQuery(con, sprintf("SELECT 1 FROM etl_file_log WHERE file_name = '%s'", file_name))
  if (nrow(file_already_loaded) > 0) {
    log_message(paste("Skipping already-loaded file:", file_name))
    next
  }
  
  log_message(paste("Processing new file:", file_name))
  tryCatch({
    df <- read.xlsx(file)
    df$source_file <- file_name
    df$upload_timestamp <- Sys.time()
    
    dbWriteTable(con, "datim_data", df, append = TRUE, row.names = FALSE)
    log_message(paste("Appended data from", file_name, "to datim_data"))
    
    dbExecute(con, "INSERT INTO etl_file_log (file_name, loaded_at) VALUES ($1, $2)",
              params = list(file_name, Sys.time()))
    
  }, error = function(e) {
    log_message(paste("Error processing", file_name, ":", e$message))
  })
}

# Log record count
record_count <- dbGetQuery(con, "SELECT COUNT(*) AS total FROM datim_data")
log_message(paste("Final record count in 'datim_data':", record_count$total))
print(record_count)

# Export full table to Excel
query_result <- dbGetQuery(con, "SELECT * FROM datim_data")
output_file_name <- paste0("Data/processed_files/extracted_data_fy24q4_", Sys.Date(), ".xlsx")
write_xlsx(query_result, output_file_name)
log_message(paste("Exported full dataset to:", output_file_name))

# Disconnect
dbDisconnect(con)
log_message("Database connection closed.")
log_message("ETL process completed.")
