# This script loads the raw datasets, merges them, renames columns for clarity, and performs initial data inspection.

# Data description:
# 1. "health_outcomes.csv": Contains health outcomes, preventative services, and unhealthy behaviors data at the county level.
# 2. "regions.csv": Contains US geography information, mapping states to regions and divisions.

# Function to load and preprocess the data
load_and_preprocess_data <- function(health_data_file = 'PLACES_county_cleanheader.csv', 
                                     region_data_file = 'us census bureau regions and divisions.csv') {
  
  # Read the health outcomes dataset
  df1 <- read.csv(health_data_file) 
  # Read the regions dataset
  df2 <- read.csv(region_data_file)
  
  # Merge the two dataframes based on state abbreviation
  # df1 is joined with df2 using 'StateAbbr' from df1 and 'State.Code' from df2
  merged_df <- merge(df1, df2, by.x = "StateAbbr", by.y = "State.Code")
  
  # Rename columns to be more concise and understandable
  colnames(merged_df) <- c("StateAbbr","Place","All Teeth Lost","Arthritis","Cancer",
                           "KidneyDisease","PulmonaryDisease","CoronaryHeartDisease",
                           "Asthma","Diabetes","HighBloodPressure","HighCholestrol",
                           "MentalHealthIssues","PhysicalHealthIssues","Stroke",
                           "CervicalCancerScreening","CholestrolScreening",
                           "LackofHealthInsurance","FecalOccultBloodTest","Mammography",
                           "ClinicalPreventiveServices(Men)",
                           "ClinicalPreventiveServices(Women)",
                           "MedicineIntakeforHighBP","DentistVisit","RoutineChcekup",
                           "BingeDrinking","Smoking","NoPhysicalAcivity","Obesity",
                           "Sleeplessness","State","Region","Division")
  
  # Output the dimensions of the merged dataframe
  cat("Number of rows in merged dataset:", nrow(merged_df), "\n")
  cat("Number of columns in merged dataset:", ncol(merged_df), "\n")
  
  # To view dataframe structure and summary
  # str(merged_df)      # Displays the structure of the dataframe (column types, etc.)
  # summary(merged_df)  # Provides a statistical summary of each column
  # head(merged_df)     # Shows the first few rows of the dataframe
  
  return(merged_df)
}