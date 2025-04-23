# Load necessary libraries
source("Scripts/Analytics/r prep2.r")

# Define the list of dataelementuid values
uids <- c("A5A8LKqJw4w", "AakrBNBU2G4", "aFOYtl2chsD", "bGCtyadJhBd", 
          "bYF6imuLSgV", "c0dzFGFAZuO", "cObJTp3DWdY", "cRCw63EQEbM", 
          "czF2lzyXksI", "ectUGvYjtaK", "fg53NvKg3EN", "fpW7iq7zFNN", 
          "geh4jV9OIh7", "H7Iu1SBCLTm", "HOtmew3ZhxO", "hvtNfA73XhN", 
          "Hyvw9VnZ2ch", "iAYee99BIjX", "in0Xpzvlr33", "IvI3KbJILcD", 
          "IyKy3G6DbZ4", "JoERp5gZ6o1", "JuMoiYn1jKB", "K3I0l3A6fNt", 
          "kegCp8t3TVW", "KNgG5EHQ69Z", "KNO4emPfF91", "KPBucMJSdii", 
          "L2n6ajKliOT", "lAXeeoyddZd", "NWnZzJD94qO", "oREygLjW7vd", 
          "Os9GkOOHHJR", "ovQaECwOS1M", "PkVxxyOLky3", "qhGxKnmrZBd", 
          "QI0LrOAmBCG", "RGS21a5msis", "RhkU5KjtAi6", "RHN2Ui10Ivu", 
          "ScQASwweWXL", "SnpwfHHrqAu", "SpjvCpxnc20", "TjPwm5FAwoE", 
          "tUWykiXBnjC", "UossaY2RAd4", "vTXUgzE9cpS", "wkMmlftfTvx", 
          "XGCkcu1hrSo", "XuHtzXGDS00", "XwrgoYZOp6F", "Y5zUjJ7a5fK", 
          "YMfWvFuB5kH")


df <- read.xlsx("Data/reserach/Data sets elements and combos paramaterized November 2024.xlsx")

df
colnames(df)

filtered_df1 <- df %>%
  filter(dataelementuid %in% uids)

#filtering out rows containing 'Unknown Age' on the colum categoryoptioncomb
filtered_df <- filtered_df1 %>%
  filter(!grepl("Unknown Age", categoryoptioncombo)) %>%
  select(dataelementuid, dataelement, dataelementdesc, shortname, categoryoptioncombo, categoryoptioncombocode)


write.xlsx(filtered_df, "Data/reserach/filtered_dataset.xlsx")

head(filtered_df)


# reading the datasets to be merged
df_filtered <- read.xlsx("Data/reserach/filtered_dataset.xlsx")
datim_file <- read.xlsx("Data/reserach/datim_file.xlsx")

datim_file
df_filtered

colnames(df_filtered)

# Select only required columns from filtered_dataset
filtered_selected <- df_filtered %>%
  select(dataelementuid, shortname, dataelementdesc, categoryoptioncombo, categoryoptioncombocode)

# Perform the left join using BOTH 'dataelementuid' and 'categoryoptioncombocode' as keys
merged_data <- datim_file %>%
  left_join(filtered_selected, by = c("dataelementuid", "categoryoptioncombocode"))


# Export the merged dataset
write.xlsx(merged_data, "Data/reserach/datim_file_updated_28march.xlsx") #Create the file with the file name to include date


#Pulling actual facility names and provinces
final_output <- read.xlsx("Data/reserach/datim_file_updated_28march.xlsx")
health_facility <- read.xlsx("Data/reserach/Insight Facilities.xlsx")

colnames(final_output)
colnames(health_facility)

# Join using OrgUnitCode
datim_file_dsd_final_28mar <- final_output %>%
  left_join(health_facility %>% select(OrgUnitCode, FacilityName, District, Province, Owner), 
            by = "OrgUnitCode")

# Save
write.xlsx(datim_file_dsd_final_28mar, "Data/reserach/final_output_with_facility_dsd_UPDATEDv1.xlsx")


#Seperating the values in
df <- read.xlsx("Data/reserach/final_output_with_facility_dsd_UPDATEDv1.xlsx")

# Define category lists
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

# Function to classify values into categories
classify_values <- function(row) {
  values <- unlist(strsplit(row, ", "))  # Split values by comma
  list(
    Age = ifelse(length(values[values %in% age_categories]) > 0, 
                 paste(values[values %in% age_categories], collapse = ", "), "N/A"),
    Sex = ifelse(length(values[values %in% sex_categories]) > 0, 
                 paste(values[values %in% sex_categories], collapse = ", "), "N/A"),
    `HIV test results` = ifelse(length(values[values %in% hiv_test_results_categories]) > 0, 
                                paste(values[values %in% hiv_test_results_categories], collapse = ", "), "N/A"),
    Modality = ifelse(length(values[values %in% modality_categories]) > 0, 
                      paste(values[values %in% modality_categories], collapse = ", "), "N/A"),
    `ART Status` = ifelse(length(values[values %in% art_status_categories]) > 0, 
                          paste(values[values %in% art_status_categories], collapse = ", "), "N/A")
  )
}

# Apply function to the "categoryoptioncombo" column
df_cleaned <- df %>%
  rowwise() %>%
  mutate(classified = list(classify_values(categoryoptioncombo))) %>%  
  unnest_wider(classified) %>%
  ungroup()

# Save the updated dataset
write_xlsx(df_cleaned, "Data/reserach/final_output_cleaned31.xlsx")

# View final result
print(df_cleaned)

