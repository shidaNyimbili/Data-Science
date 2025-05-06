# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(writexl)

# Load helper script
source("Scripts/Analytics/r prep2.r")
uids <- source("Scripts/Data Science/datim uids.r")

uids
# Paths
input_folder <- "Data/raw_files/"
output_folder <- "Data/processed_files/"
files_to_process <- list.files(input_folder, pattern = "^fy[0-9]{2}q[1-4].*\\.xlsx$", full.names = TRUE)


ref_df <- read_excel(file.path(input_folder, "Data sets elements and combos paramaterized November 2024.xlsx")) %>%
  filter(dataelementuid %in% uids, !str_detect(categoryoptioncombo, "Unknown Age")) %>%
  select(dataelementuid, dataelement, dataelementdesc, shortname, categoryoptioncombo, categoryoptioncombocode)

facility_info <- read_excel(file.path(input_folder, "Insight Facilities.xlsx")) %>%
  select(OrgUnitCode, FacilityName, District, Province, Owner)

# Categories
age_categories <- c("0=<2Months (<2 Months)", "2-12 Months", "<1", "1-4", "5-9", "10-14", "15-19 (15-17 OVC)", "20-24",
                    "25-29", "30-34", "35-39", "40-44", "45-49", "50+", "50-54", "55-59", "60-64", "65+")
sex_categories <- c("Male", "Female")
hiv_test_results_categories <- c("Known Positives", "Negative", "New Negative", "Newly Tested Positives", "Positive", 
                                 "PrEP Test Result - Negative", "PrEP Test Result - Other", "Unknown")
modality_categories <- c("<200 CD4", ">=200 CD4", "ARV Dispensing Quantity - 3 to 5 months", 
                         "ARV Dispensing Quantity - 6 or more months", "ARV Dispensing Quantity - Less than 3 months",
                         "Breastfeeding", "CD4 Not Eligible", "CD4 Unknown", "Directly-Assisted", "EID First Test",
                         "EID Second Test or more", "No Contact Outcome - Died", 
                         "No Contact Outcome - Interruption in Treatment (<3 Months Treatment)",
                         "No Contact Outcome - Interruption in Treatment (3-5 Months Treatment)",
                         "No Contact Outcome - Interruption In Treatment (6+ Months Treatment)",
                         "No Contact Outcome - Refused (Stopped) Treatment", "No Contact Outcome - Transferred Out",
                         "Pregnant", "Unassisted", "COD: HIV Disease Resulting in TB", "COD: Other HIV Disease",
                         "COD: Other Natural Causes", "COD: Unknown Cause", "Facility Distribution",
                         "No Contact Outcome - Interruption in Treatment (<3 Months Interruption)",
                         "No Contact Outcome - Interruption in Treatment (3-5 Months Interruption)",
                         "No Contact Outcome - Interruption In Treatment (6+ Months Interruption)",
                         "Pregnant", "PrEP Type - Oral", "Surgical Technique", "Unassisted - Self", 
                         "Unassisted - Sex Partner")
art_status_categories <- c("Already", "New")

# Faster vectorized classification
classify_columns <- function(combo_vector) {
  list(
    Age = str_extract_all(combo_vector, str_c(age_categories, collapse = "|")),
    Sex = str_extract_all(combo_vector, str_c(sex_categories, collapse = "|")),
    `HIV test results` = str_extract_all(combo_vector, str_c(hiv_test_results_categories, collapse = "|")),
    Modality = str_extract_all(combo_vector, str_c(modality_categories, collapse = "|")),
    `ART Status` = str_extract_all(combo_vector, str_c(art_status_categories, collapse = "|"))
  )
}

# Ensure output folder exists
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# Processing loop
walk(files_to_process, function(file_path) {
  file_name <- basename(file_path)
  message("Processing: ", file_name)
  
  # Read file
  datim <- read_excel(file_path)
  
  # Rename if needed
  names(datim)[names(datim) == "DataElement"] <- "dataelementuid"
  names(datim)[names(datim) == "CategoryOptionComboCode"] <- "categoryoptioncombocode"
  
  # Merge with lookup tables
  merged <- datim %>%
    left_join(ref_df, by = c("dataelementuid", "categoryoptioncombocode")) %>%
    left_join(facility_info, by = "OrgUnitCode")
  
  # Vectorized classification
  classifications <- classify_columns(merged$categoryoptioncombo)
  final <- bind_cols(merged, as_tibble(classifications))
  
  # Save result
  fy_qtr <- str_extract(file_name, "^fy[0-9]{2}q[1-4]")
  processed_date <- format(Sys.Date(), "%Y-%m-%d")
  out_file <- paste0(fy_qtr, "_processed_", processed_date, ".xlsx")
  
  write_xlsx(final, file.path(output_folder, out_file))
  message("✅ Saved: ", out_file)
})
