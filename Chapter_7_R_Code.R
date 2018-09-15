# Load packages required for later code
library(foreign)
library(data.table)
library("ggplot2")

# Read in RRS data for use in plots
rrs_database <- fread("https://raw.githubusercontent.com/SurveyDataAnalysis/supplemental/master/Leuven%20rumination%20data.csv", data.table = FALSE)
rrs_working_data <- rrs_database
rrs_working_data <- subset(rrs_database, select = c(
  rrs1, rrs2, rrs3, rrs4, rrs5,
  rrs6, rrs7, rrs8, rrs9, rrs10,
  AGE, gender, dass_21_d_prorated_pomp, hx_depression 
))

sum_score <- function(items, min_num_items = ncol(items)) {
  items <- as.data.frame(items)
  num_items <- NCOL(items)
  n <- NROW(items)
  score <- vector(mode = "numeric", length = n)
  for (i in 1:n){
    if (sum(!is.na(items[i, ])) >= min_num_items) {
      score[i] <- num_items * rowMeans(items[i, ], na.rm = TRUE)
    }
    else {
      score[i] <- NA
    }
  }
  return(score)
}

pomp_from_score <- function(score, min_possible, max_possible) {
  if (min_possible >= max_possible) {
    stop("Error - min_possible must be less than max_possible")
  }
  pomp <- (score - min_possible) / (max_possible - min_possible) * 100
  return(pomp)
}
rrs_tot_score <- sum_score(rrs_working_data[, c(1:10)], 8)
rrs_total_pomp <- pomp_from_score(rrs_tot_score, 10, 40)

fit <- lm(rrs_total_pomp ~ hx_depression + gender, data = rrs_working_data,
          singular.ok = FALSE)
fit

# Next create scores for brooding and reflection
# Note the column selection within the brackets
brooding <- sum_score(rrs_working_data[, c(2, 5, 6, 9, 10)], 4)
reflection <- sum_score(rrs_working_data[, c(1, 3, 4, 7, 8)], 4)

reflection_pomp <- pomp_from_score(reflection, 5, 20)
brooding_pomp <- pomp_from_score(brooding, 5, 20)

# Bar chart example using RRS data (CHECKED)
rrs1_bar_chart <- ggplot(rrs_working_data, aes(rrs1)) + geom_bar(color = "black", fill = "blue") +
  scale_x_discrete(name = "RRS1:  Analyze recent events to try to understand\n why you are depressed", 
                   limits = c("Almost, never", "Sometimes", "Often", "Almost always")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
rrs1_bar_chart

# Scatterplot of depression & rumination data (CHECKED)
rrs1_scatterplot <- ggplot(rrs_database, aes(rrs_total_pomp, dass_21_d_prorated_pomp)) + 
  geom_point(alpha = 1 / 10) +
  theme_classic() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_smooth(method='lm', se=FALSE) +
  xlab("Rumination") +
  ylab("Depression")
rrs1_scatterplot

# Segregation of RRS data by the factor gender
rrs_working_data$gender <- factor(rrs_database$gender,
                              levels = c(0, 1),
                              labels = c("female", "male"),
                              ordered = FALSE)

# Scatterplot of depression & rumination with faceting by gender (CHECKED)
rrs1_scatterplot <- ggplot(rrs_database, aes(rrs_total_pomp, dass_21_d_prorated_pomp)) + 
  geom_point(alpha = 1 / 10) +
  theme_classic() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_smooth(method='loess', se=FALSE) +
  xlab("Rumination") +
  ylab("Depression")
rrs1_scatterplot + facet_wrap(~gender)

# Boxplot of brodding data separated by a factor of gender
rrs_boxplot <- ggplot(data = rrs_working_data,
                      mapping = aes(rrs_working_data$gender,brooding_pomp))
rrs_boxplot + geom_boxplot(data=NULL,
                           stat = "boxplot", position = "dodge",
                           outlier.shape = 19, outlier.size = 1.5,
                           outlier.stroke = 0.5, notch = FALSE,
                           notchwidth = 0.5, varwidth = TRUE, na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE) +
  labs(x = "Gender", y = "Brooding POMP Units") + stat_boxplot(geom ='errorbar')

# Dot plot of RRS reflection POMP scores
rrs_dotplot <- ggplot(rrs_working_data, aes(x = reflection_pomp, fill = factor(gender))) + geom_dotplot(binwidth = 1.8, dotsize = 0.6)
rrs_dotplot + xlab("Reflection") + scale_y_continuous(NULL, breaks = NULL)

# Read in a CSV file of rumination data from GitHub (CHECKED)
table_database <- fread("https://raw.githubusercontent.com/SurveyDataAnalysis/supplemental/master/Chapter_7_Table_Data.csv", data.table = FALSE)

# Subset the data to only include depression, brooding and rumination scores, and age
# Also remove missing cases (CHECKED)
table_working_data <- subset(table_database, select = c(AGE, DASS21_D_T1, rrs_brooding, rrs_reflection))
table_working_data_complete <- na.omit(table_working_data)

# Calculate summary statistics on the data set using the lapply function
# Output the results to a data frame (CHECKED)
table_summary_stats <- lapply( table_working_data_complete, function(x) rbind( Mean = mean(x) , SD = sd(x) , Median = median(x) , Minimum = min(x) , Maximum = max(x)))
table_summary_df <- data.frame(table_summary_stats)

# Create a vector of human-readable column names for in table (CHECKED)
table_summary_colnames <- c("Age (Yrs)","DASS: Depression", "RRS: Brooding", "RRS: Reflection")

# Use the kable function to build table from all previously-created data objects (CHECKED)
library(knitr)
kable(table_summary_df, digits = 1, col.names = table_summary_colnames, caption = "Summary Statistics for RRS Study Data")

# Apply nicer formatting to previously-created table (NOT WORKING)
library(kableExtra)
kable_styling(table_summary_df, bootstrap_options = c("hover", "condensed"))
