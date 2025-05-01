library(RPostgreSQL)


source("Scripts/Data Science/r prep2.r")


# Connect to default Postgres DB to check/create the health_data DB
con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'arcgis_pro',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'Sh1d@804011')

# # Connect
# con <- dbConnect(PostgreSQL(),
#                  dbname = "arcgis_pro",
#                  host = "localhost",
#                  port = 5432,
#                  user = "your_username",
#                  password = "Sh1d@804011")

# List all tables
dbListTables(con)

# Example: View data from a table
data <- dbGetQuery(con, "SELECT * FROM your_table_name")
head(data)
