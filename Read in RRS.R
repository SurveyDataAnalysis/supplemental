# Read in data on Ruminative Responses Scale (RRS) ------------------------

# Thanks to Rbloggers for writing about the data.table package
# https://www.r-bloggers.com/getting-data-from-an-online-source/

# These two lines can be commented out if these two packages
# are already installed:
install.packages("curl")
install.packages("data.table")

library(data.table) 

# The URL for the data is :
# https://github.com/SurveyDataAnalysis/supplemental/blob/master/Leuven%20rumination%20data.csv
# Read the data directly from GitHib using fread()
rrs_database <- fread("https://raw.githubusercontent.com/SurveyDataAnalysis/supplemental/master/Leuven%20rumination%20data.csv",
                      data.table = FALSE)

# Clean up your workspace by unloading packages when you're finished
# Note: This is command that RStudio runs when you uncheck a package
# in the Packages tab
detach("package:data.table", unload = TRUE)

# So, rrs_database is the original database.
# We must remember to save both the raw and intermediate forms

# Remember to save both raw and intermediate forms
# Let us make a ***copy*** of the data now and make modifications 
# only to the copy. We will call the copy
# rrs_working_data

rrs_working_data <- rrs_database

# Print a helpful message for the reader.
cat("You now have two new files in you R Environment.\n\n",
    "You have rrs_database, which is the original file.\n",
    "You also have rrs_working_data, a copy of the original file.\n\n",
    "Only manipulate and make changes to the working file, rrs_working_data.\n",
    "This way, you can always refer back to the original data to help track down errors.")