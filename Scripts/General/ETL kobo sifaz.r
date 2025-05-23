# SIFAZ ETL: KoboToolbox dummy data → PostgreSQL/PostGIS with logging
source("Scripts/Analytics/r prep2.r")

# --- Logging Setup ---
log_file <- "log/etl_log.txt"
if (!dir.exists("log")) dir.create("log")
log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  write(paste0("[", timestamp, "] ", msg), file = log_file, append = TRUE)
}
log_message("🚀 ETL run started.")

tryCatch({
  
  # --- Secure Password ---
  password <- Sys.getenv("PG_PASSWORD")
  
  # --- Load Kobo Dummy Data ---
  df <- read_excel("Data/kobo_dummy_data_sifaz.xlsx")
  df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
  st_geometry(df_sf) <- "geom"
  
  # --- Connect to 'postgres' to ensure 'sifaz_dw' exists ---
  con <- dbConnect(RPostgres::Postgres(),
                   dbname = 'postgres',
                   host = 'localhost',
                   port = 5432,
                   user = 'postgres',
                   password = password)
  
  db_check_result <- dbGetQuery(con, "SELECT 1 FROM pg_database WHERE datname = 'sifaz_dw'")
  if (nrow(db_check_result) == 0) {
    dbExecute(con, "CREATE DATABASE sifaz_dw")
    log_message("✅ Created database 'sifaz_dw'.")
  } else {
    log_message("ℹ️ Database 'sifaz_dw' already exists.")
  }
  dbDisconnect(con)
  
  # --- Connect to sifaz_dw ---
  con <- dbConnect(RPostgres::Postgres(),
                   dbname = 'sifaz_dw',
                   host = 'localhost',
                   port = 5432,
                   user = 'postgres',
                   password = password)
  
  # --- Enable PostGIS ---
  postgis_check <- dbGetQuery(con, "SELECT * FROM pg_extension WHERE extname = 'postgis'")
  if (nrow(postgis_check) == 0) {
    dbExecute(con, "CREATE EXTENSION postgis;")
    log_message("✅ Enabled PostGIS extension.")
  } else {
    log_message("ℹ️ PostGIS already enabled.")
  }
  
  # --- Drop and recreate table ---
  dbExecute(con, "DROP TABLE IF EXISTS kobo_interventions CASCADE;")
  dbExecute(con, "
  CREATE TABLE kobo_interventions (
      submission_id SERIAL PRIMARY KEY,
      enumerator TEXT,
      province TEXT,
      district TEXT,
      ward TEXT,
      activity_type TEXT,
      beneficiary_count INT,
      cost_usd FLOAT,
      yield_impact_score FLOAT,
      submission_time TIMESTAMP,
      notes TEXT,
      latitude DOUBLE PRECISION,
      longitude DOUBLE PRECISION,
      geom GEOMETRY(Point, 4326)
  );"
  )
  log_message("🗂️ Table 'kobo_interventions' recreated.")
  
  # --- Load data ---
  st_write(df_sf, con, layer = "kobo_interventions", append = TRUE)
  log_message("✅ Data loaded successfully into 'kobo_interventions'.")
  
  dbDisconnect(con)
  log_message("✅ ETL process completed successfully.")
  
}, error = function(e) {
  log_message(paste("❌ ETL failed:", e$message))
})
