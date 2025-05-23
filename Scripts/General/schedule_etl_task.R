
# Schedule ETL Script using taskscheduleR (Windows)

library(taskscheduleR)

# Set full path to your ETL script
etl_script_path <- "C:/Users/SNyimbili/
OneDrive - Right to Care/Documents/RTCZ/RTCZ/R/Data_Science
/Scripts/General/ETL kobo sifaz.r"

# Set path to Rscript.exe (adjust version if needed)
rscript_exe <- "C:/Program Files/R/R-4.3.2/bin/Rscript.exe"

# Schedule the task to run daily at 03:00
taskscheduler_create(
  taskname = "etl_sifaz_kobo",
  rscript = etl_script_path,
  schedule = "DAILY",
  starttime = "15:00",
  Rexe = rscript_exe
)

cat("✅ Scheduled ETL job 'etl_sifaz_kobo' to run daily at 01500 using Task Scheduler.\n")
