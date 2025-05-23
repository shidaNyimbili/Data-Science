# library(cronR)
# 
# install.packages("taskscheduleR")
# 
# install.packages("cronR", type = "source", repos = "http://cran.us.r-project.org")
# 
# # Path to your saved R script
# r_script <- "~/Projects/SIFAZ/scripts/etl_kobo_to_sifaz_dw_cron_ready.R"  # update to your actual path
# 
# # Generate cron command
# cmd <- cron_rscript(r_script)
# 
# # Schedule the job
# cron_add(command = cmd,
#          frequency = 'daily',
#          at = '3AM',
#          id = 'etl_sifaz_kobo',
#          description = 'Daily ETL from Kobo dummy data to PostGIS')


library(taskscheduleR)

# Schedule your ETL R script
taskscheduler_create(
  taskname = "etl_sifaz_kobo",
  rscript = "C:/Users/SNyimbili/OneDrive - Right to Care/Documents/RTCZ/RTCZ/R/Data Science/Scripts/General/ETL kobo sifaz.r",
  schedule = "DAILY",
  starttime = "15:00",
  Rexe = "C:/Program Files/R/R-4.3.1/bin/Rscript.exe"
)