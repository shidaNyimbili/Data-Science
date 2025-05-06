#Load packages
source("scripts/r prep2.r")



# Read the dataset (adjust file path if needed)
df <- tryCatch({
  data4 <- read.xlsx("Data/reserach/Sample kobo.xlsx")  # Adjust path if necessary
}, error = function(e) {
  stop("Error reading the dataset. Please check the file path and format.")
})

# View the dataset (for debugging purposes)
view(df)

# Ensure 'Date.of.entry' is in the correct date format and handle any errors
df$`Date.of.entry` <- tryCatch({
  convert_excel_date <- function(excel_date) {
    as.Date(excel_date, origin = "1899-12-30")  # Excel's origin date is 1899-12-30
  }
  convert_excel_date(df$`Date.of.entry`)  # Apply the conversion
}, error = function(e) {
  stop("Error converting 'Date.of.entry' to date format. Please check the format.")
})

# Check if 'Date.of.entry' column exists
if (!"Date.of.entry" %in% colnames(df)) {
  stop("The dataset does not contain a 'Date.of.entry' column.")
}

# Ensure there are no missing values in the 'Date.of.entry' column
df <- df %>% filter(!is.na(`Date.of.entry`))

# Identify the most recent submission
latest_entry <- df %>% filter(`Date.of.entry` == max(`Date.of.entry`, na.rm = TRUE))

# Ensure there are previous entries to update
if (nrow(latest_entry) == 0) {
  stop("No entries found in the dataset.")
}

# Extract the facility name, district, and province from the most recent submission
facility_name <- latest_entry$`Facility:`[1]
district_name <- latest_entry$`District:`[1]
province_name <- latest_entry$`Province:`[1]

# Identify previous submissions for the same facility, district, and province (excluding the latest entry)
previous_entries <- df %>% 
  filter(`Facility:` == facility_name, 
         `District:` == district_name, 
         `Province:` == province_name, 
         `Date.of.entry` != max(`Date.of.entry`, na.rm = TRUE))

# If no matching previous entries, notify the user
if (nrow(previous_entries) == 0) {
  stop("No previous entries found for the same facility, district, and province.")
}

df

# Select relevant columns from the latest entry
columns_to_update <- c(
  "What.is.the.TX.CURR:.(.After.making.updates.&.working.on.backlog)",
  "TX.New:(.After.making.updates.&.working.on.backlog)",
  "Current.due.for.VL:(.After.making.updates.&.working.on.backlog)",
  "Eligible.for.routine.VL:.(.After.making.updates.&.working.on.backlog)",
  "TX.PVLS.Numerator:.(.After.making.updates.&.working.on.backlog)",
  "TX.PVLS.Denominator:.(.After.making.updates.&.working.on.backlog)",
  "Death:.(.After.making.updates.&.working.on.backlog)",
  "Transout:.(.After.making.updates.&.working.on.backlog)",
  "IIT:.(.After.making.updates.&.working.on.backlog)",
  "Stopped:.(.After.making.updates.&.working.on.backlog)",
  "Trans.In:.(.After.making.updates.&.working.on.backlog)",
  "Refused:.(.After.making.updates.&.working.on.backlog)"
)

# Ensure no NA values in columns_to_update before assignment (optional: handle them as needed)
latest_entry_clean <- latest_entry[, columns_to_update, drop = FALSE]
latest_entry_clean[is.na(latest_entry_clean)] <- 0  # Example: replace NAs with 0

# Replace values in previous submissions with those from the latest submission
previous_entries[, columns_to_update] <- latest_entry_clean

# Combine the updated previous entries and remove the latest submission
df_final <- bind_rows(previous_entries, latest_entry)

# Save the cleaned dataset
tryCatch({
  write_xlsx(df_final, "Data/reserach/cleaned_datasetkobo.xlsx")
}, error = function(e) {
  stop("Error writing the cleaned dataset. Please check file permissions.")
})

print("Dataset updated successfully. New submission removed.")

