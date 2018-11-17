# Read in full Leuven study data from CSV file
Leuven_Full_Data <- read.csv(file.choose())
Subset_Int <- data.frame(Leuven_Full_Data)

# Subset the data to only include Avoidance-Rumination scores, Social Impairment scores, and age
# Also remove missing cases
table_working_data <- subset(Subset_Int, select = c(AGE, bads_prorated_ar_pomp, bads_prorated_si_pomp))
table_working_data_complete <- na.omit(table_working_data)

# Calculate summary statistics on the data set using the lapply function
# Output the results to a data frame
table_summary_stats <- lapply( table_working_data_complete, function(x) rbind( Mean = mean(x) , SD = sd(x) , Median = median(x) , Minimum = min(x) , Maximum = max(x)))
table_summary_df <- data.frame(table_summary_stats)
table_summary_df

# Create a vector of human-readable column names for in table
table_summary_colnames <- c("Age (Yrs)", "Avoidance-Rumination", "Social Impairment")

# Use the kable function to build table from all previously-created data objects (CHECKED)
library(knitr)
table_kable <- knitr::kable(table_summary_df,
                            digits = 1,
                            col.names = table_summary_colnames,
                            caption = "Summary Statistics for  Study Data",
                            format = "markdown")
table_kable

# Apply nicer formatting to previously-created table
library(kableExtra)
table_html <- knitr::kable(table_summary_df,
                           digits = 1,
                           col.names = table_summary_colnames,
                           caption = "Summary Statistics for Behavioral Approach-Avoidance and Distress Scale Data",
                           format = "html")
kable_styling(table_html)
