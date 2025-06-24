# This script performs exploratory data analysis (EDA) on the preprocessed data.
# It involves creating subsets of data (Tables 1-5) and generating visualizations, insights.

# Function to perform EDA
perform_eda <- function(merged_df) {
  
  # Table1 - Which health outcome is predominant in USA?
  # Selects columns representing various health outcomes (columns 3 to 12 and 15).
  Table1 <- merged_df[,c(3:12,15)] 
  # Calculate column means for these health outcomes.
  colmeans_t1 <- colMeans(Table1, na.rm = TRUE) 
  # Sort the means in descending order.
  sorted_means_t1 <- sort(colmeans_t1, decreasing = TRUE) 
  # Define colors for the bar plot (highlighting the first bar).
  colors_t1 <- c(rep("skyblue", 1), rep("grey", length(sorted_means_t1) - 1)) 
  
  # Adjust plot margins to accommodate long x-axis labels.
  par(mar=c(10, 4, 4, 2) + 0.1) 
  # Create a bar plot of average proportions of health outcomes.
  barplot(sorted_means_t1, main='Average Proportions of Health Outcomes', 
          ylab='Average Proportions', ylim = c(0,35), col=colors_t1, las=2) # las=2 makes labels perpendicular to axis
  # Reset plot margins to default.
  par(mar=c(5, 4, 4, 2) + 0.1) 
  
  # Table2 - Unhealthy behaviors and lack of preventive measures contributing to High BP among US population
  # Selects columns for High Blood Pressure, and various unhealthy behaviors and preventive measures (columns 11, and 16 to 30).
  Table2 <- merged_df[,c(11,16:30)]
  
  # Generate a corrgram to visualize correlations between variables in Table2.
  # lower.panel shows scatter points, upper.panel shows confidence ellipses, diag.panel shows density plots.
  corrgram(Table2, 
           main="Correlation Matrix of HighBP and Related Factors",
           lower.panel=panel.pts, upper.panel=panel.conf, diag.panel=panel.density)
  # Note: MedicineIntakeforHighBP,RoutineCheckup,Smoking,NoPhysicalActivity,Sleeplessness are having stronger correlation with HighBP
  
  # Table3 - Regional disparities in High BP Prevalence
  # Selects columns for High Blood Pressure and Region.
  Table3 <- merged_df[,c(11,32)]
  # Calculate the average HighBloodPressure for each region.
  result_table3 <- Table3 %>%
    group_by(Region) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) # na.rm=TRUE ignores NA values in mean calculation
  
  # Prepare data for the boxplot.
  region_bp_data <- merged_df %>%
    dplyr::select(Region, HighBloodPressure)
  
  # Create a boxplot showing the distribution of HighBloodPressure by Region.
  p_boxplot_region <- ggplot(region_bp_data, aes(x = Region, y = HighBloodPressure, fill = Region)) +
    geom_boxplot() +
    labs(title = "High Blood Pressure Distribution by Region", x = "Region", y = "High Blood Pressure") +
    theme_minimal() # Use a minimal theme for the plot
  print(p_boxplot_region)
  
  # Table 4 - Average High BP proportions across different states in 'south' region
  # Filter data for the 'South' region and select State and HighBloodPressure columns.
  Table4 <- merged_df[merged_df$Region=='South',c(31,11)]
  # Calculate the average HighBloodPressure for each state within the South region.
  result_table4 <- Table4 %>%
    group_by(State) %>%
    summarise(across(everything(), mean, na.rm = TRUE))
  
  # Adjust plot margins for long x-axis labels.
  par(mar=c(12, 4, 4, 2) + 0.1) 
  # Create a bar plot of average HighBloodPressure by state in the South region.
  barplot(result_table4$HighBloodPressure, 
          main='Avg Pop. proportion with HighBP by State in South Region', 
          ylab='Average Proportions', names.arg = result_table4$State, las=2, cex.names=0.7) # cex.names adjusts label size
  # Reset plot margins.
  par(mar=c(5, 4, 4, 2) + 0.1) 
  
  # Table 5 - Average High Blood Pressure proportions across different states in US
  # Selects State and HighBloodPressure columns for all data.
  Table5 <- merged_df[,c(31,11)]
  # Calculate the average HighBloodPressure for each state.
  result_table5 <- Table5 %>%
    group_by(State) %>%
    summarise(across(everything(), mean, na.rm = TRUE))
  
  # Create a categorical variable 'YRange' by cutting HighBloodPressure into bins for coloring the plot.
  # 'right = FALSE' means intervals are [a, b). 'Inf' ensures all higher values are included.
  result_table5$YRange <- cut(result_table5$HighBloodPressure, breaks = c(0, 35, 40, 45, Inf), labels = FALSE, right = FALSE)
  
  # Create a line plot of average HighBloodPressure by State, ordered by HighBloodPressure.
  # 'group = 1' is necessary for geom_line when x-axis is categorical but you want a single line.
  p_lineplot_state <- ggplot(result_table5, aes(x = reorder(State, HighBloodPressure), y = HighBloodPressure, color = YRange, group = 1)) +
    geom_line() +
    geom_point() +
    geom_text(aes(label = State), nudge_y = 0.02, check_overlap = TRUE, size=3) + # Adds state labels, avoids overlap
    labs(title = 'HighBloodPressure by State',
         x = 'State',
         y = 'Average population proportion with HighBloodPressure') +
    scale_color_gradient(low = "blue", high = "red") + # Color gradient for YRange
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=8)) # Rotate x-axis labels for readability
  print(p_lineplot_state)
  
  
  return(list(Table1=Table1, Table2=Table2, Table3=Table3, Table4=Table4, Table5=Table5))
}