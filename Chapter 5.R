# Chapter 5
# Exploring and describing survey data

# Install and load packages -----------------------------------------------

# If needed, install packages
install.packages("ggplot2")
install.packages("corrplot")

# Load some packages to use
library(ggplot2) # For figures
library(corrplot) # Specifically for correlation heatmaps


# Read in the RRS data ----------------------------------------------------

source("Read in RRS.R")

# Feel free at this point to read in some of your own data
# - or data found in an online repository - to follow along
# with these techniques.


# Begin looking at the data -----------------------------------------------

# View is sometimes slow with large dataframes, but handly nonetheless.
View(rrs_working_data)
#
# We see some things right away in the RRS data:
# Gender is 0 and 1; they are not labelled, so we should check the data dictionary.
# There is an NA visible; there are probably more as well, so we should look.
#
# You can output any dataframe to the console by entering its name,
# but often this will be too much information to look at.
rrs_working_data

# You can take a quick look at the top of the dataframe using head()
head(rrs_working_data)

# You can use arguments with head, as well as subset the dataframe if you wish
head(rrs_working_data, n = 4)
head(rrs_working_data[1:10], n = -610)

# On your own, try the tail() function 

# See what variables are included in the dataframe
names(rrs_working_data)

# Is the number of rows what we expect?
nrow(rrs_working_data)

# Create a subset dataframe containing only the RRS items
rrs <- rrs_working_data[,
                        c("rrs1", "rrs2", "rrs3", "rrs4",
                          "rrs5", "rrs6", "rrs7", "rrs8",
                          "rrs9", "rrs10")]

# Make a frequency table for each item
apply(rrs, 2, function(x) table(x, useNA = "always"))

# An alternative table() method
apply(rrs, 2, table, useNA = "always")

# You can also look at proportions using prop.table()
# Frequency table 
table(rrs$rrs1, useNA = "always")

# Proportion table
proportions(table(rrs$rrs1, useNA = "always"))

# Have a quick look at the two-way table.
# This can illuminate the degress of association.
# Look at the table; where is it most vs. least dense?
table(rrs$rrs1, rrs$rrs2)

# First, create an R object called rrs1_bar_chart
rrs1_bar_chart <- ggplot(rrs, aes(rrs1)) + 
  geom_bar(color = "black",
           fill = "white")

# Next, run the name of the object to display the chart
rrs1_bar_chart

# Cleaning up the bar chart
rrs1_bar_chart <- ggplot(rrs, aes(rrs1)) + 
  geom_bar(color = "black", fill = "white") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black")) +
  scale_x_discrete(name ="RRS1: Analyze recent events to try to understand why you are depressed", 
                 limits=c("Almost never", "Sometimes", "Often", "Almost always"))

# Display the chart
rrs1_bar_chart

# Try a quick histogram
hist(rrs$rrs1)

# Try out rank()
# on the first 10 cases in rrs
# looking only at the first column
rrs[1:10, 1]
rank(rrs[1:10, 1])
# What is the mean of ranks 4 through 10?
mean(4:10)

# Make a Pearson correlation matrix
Pearson_r_rrs <- cor(rrs,
                     use = "complete.obs")

# Make a Spearman correlation matrix
Spearman_rho_rrs <- cor(rrs,
    use = "complete.obs",
    method = "spearman")

# Round to two digits to make the matrices
# easier to look at in the console
round(Pearson_r_rrs,
      digits = 2)

round(Spearman_rho_rrs,
      digits = 2)

# Make a quick histogram of the correlations in the matrix
# Just look at the lower triangle of the matrix
hist(Pearson_r_rrs[lower.tri(Pearson_r_rrs)])

# The average correlation size looks
# to be between .2 and .3 (small to medium by Cohen's conventions)
# There is quite a range, but no negative values

# Make a quick heatmap
heatmap(Pearson_r_rrs, Rowv = NA, symm = TRUE)

# Correlation heatmaps using corrplot
# Try making a correlation plot using the default
# arguments in the corrplot() function
corrplot(Pearson_r_rrs)

# Try again tweaking some of the arguments
corrplot(Pearson_r_rrs,
         method = "color",
         diag = FALSE,
         tl.col = "black",
         type = "lower")


# Score the RRS -----------------------------------------------------------

# You will need to read in the file
# named "Chapter 4 functions.R"

# If the file is saved in your working directory,
# simply use:
source("Chapter 4 functions.R")

# If the file is saved elsewhere,
# you will need to specify the path:
source("C:/Users/james/Google Drive/Survey Book/Writing/Ch. 4_ Scoring questionnaires/Chapter 4 functions.R")

# Remember, you can check your working directory with getwd()
# After checking with getwd(), you can change it with setwd()


# Calculate sumscores for the RRS -----------------------------------------

# In the rrs dataframe, create a column for the summed score
# (also known as a raw score) of the RRS
rrs$rrs_sum <- score_surveys(rrs[1:10])

# Create a column for the POMP scores
rrs$rrs_pomp <- pomp(rrs$rrs_sum, 10, 40)

# Create z scores
rrs$rrs_z <- scale(rrs$rrs_sum)

# Take a quick look at the rrs
head(rrs)

# Have a quick look at a histogram using base R graphics
hist(rrs$rrs_sum)

# Look at missing values in the sum, POMP, and z scores -------------------

# Note: When we made the tables above, we already saw how many items
# were missing within the individual items.

# Which rows have missing data for the RRS sum score?...
# Note: The code below returns the row number of the cases where
# is.na(rrs$rrs_sum) is TRUE
which(is.na(rrs$rrs_sum))

# ...or have a look at the cases with missing sum scores
rrs[which(is.na(rrs$rrs_sum)), ]

# ...or use the !complete.cases() to view the
# "not complete cases" in the console
rrs[which(!complete.cases(rrs[1:10])), ]


# Some basic descriptive statistics ---------------------------------------

summary(rrs)

# ...and some more on just the POMP scores.
# Note: You can try more on your own.
# Statistics for central tendency
mean(rrs$rrs_pomp, na.rm = TRUE)
median(rrs$rrs_pomp, na.rm = TRUE)

# Statistics for spread or dispersion
sd(rrs$rrs_pomp, na.rm = TRUE)
var(rrs$rrs_pomp, na.rm = TRUE)
range(rrs$rrs_pomp, na.rm = TRUE)
IQR(rrs$rrs_pomp, na.rm = TRUE)

# Also
min(rrs$rrs_pomp, na.rm = TRUE)
max(rrs$rrs_pomp, na.rm = TRUE)

nrow(rrs[which(rrs$rrs_sum >= 30), ])

# Are there any cases with z scores greater
# than 3 or less than -3? 
rrs[!is.na(rrs$rrs_z) &
      (rrs$rrs_z < -3 | rrs$rrs_z > 3),
    c("rrs_sum", "rrs_z")]


# Answers to Exercises ----------------------------------------------------

# Exercises
#1a
rrs <- subset(rrs_working_data, select = rrs1:rrs10)
#1b
rrs <- rrs_working_data[, 1:10]
rrs <- rrs_working_data[1:10]

#2
summary(rrs)
summary(rrs_working_data)

#3
str(rrs)
str(rrs_working_data)
# For fun, try some functions like:
str(head)

#4
# Compare the following:
table(rrs[, c("rrs1", "rrs2")])
table(rrs[, 1:2])

table(rrs$rrs1, rrs$rrs2)
table(rrs$"rrs1", rrs$"rrs2")
table(rrs[[1]], rrs[[2]])

# The first two include the variable names.
# There are no variable names in the latter three,
# because they access the elements (i.e., the contents)
# of rrs directly. The names are not included
# using these means of indexing.
#
# This can be confusing for people without a programming
# background (like us!), but it helps to try out these
# different methods when working wiht R to get a feel for
# R's behavior.
#
# It may help to read about how R uses indexing
# https://cran.r-project.org/doc/manuals/R-lang.html#Indexing

# Lists are a datatype in R.
# Lists are basically collections of things, and the
# things can be accessed as long as you refer to the correct
# index for the things you're looking for.
# It is helpful to recognize the dataframes are
# a type of list, for example:
is.list(rrs)

# One last thing, if you wish you can control the names
# in the table using the dnn argument like so:
table(rrs[, c("rrs1", "rrs2")],
      dnn = c("Item 1", "Item 2"),
      useNA = "always")

