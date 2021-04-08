# score_surveys function --------------------------------------------------
# Description: Takes a dataframe of numeric items and calculates a score
# by multiplying the number of items by the mean item response.
# If no items are missing, this is equivalent to summing the items.
# If one or more items are missing, this is equivalent to treating
# the missing items as the mean value of the non-missing items,
# and then summing all of the items. This practice is known as 
# "prorating". A score is only returned if the minimum number 
# of items (or greater) is present.
#
# Arguments are:
# items  - A dataframe containing the responses.
# Note: If items is not a dataframe, the function will 
# stop and return an error message.
#
# min_num_items - The minimum number of items for scoring the case. 
# If the length of items is less than this value, then NA will be returned.
# By default, i.e., if the user does not supply min_num_items, then 
# all of the items will be required in order to calculate a score.

score_surveys <- function (items, min_num_items = ncol(items)) {
  # Handle some possible errors
  if (!is.data.frame(items)) {
    stop("This function only handles dataframes of numeric survey data.\n Try again with items as a dataframe.")
  }
  if (min_num_items > ncol(items)) {
    stop("The argument min_num_items is larger than the number of items.")
  }
  if (min_num_items <= 0) {
    stop("The argument min_num_items should not be zero or negative.")
  }
  # Done handling errors, so apply scoring algorithm below.
  apply(X = items,
        MARGIN = 1,
        FUN = function(one_survey)
          ifelse(test = sum(!is.na(one_survey)) >= min_num_items,
                 yes = length(one_survey) * mean(one_survey, na.rm = TRUE),
                 no = NA))
}

# score_a_survey ----------------------------------------------------------
# This function takes a single vector of survey items as input.
# It returns the mean of the items multiplied by the number of items.
# In other words, it returns a prorated score, but only
# if the minimum number of required items is present.
#
# Arguments:  items - A vector containing numeric survey items
#             min_num_items - The minimum number of items required in 
#             order to calculate a score. Otherwise the score will be
#             left as NA.
# Returns: A single prorated score, equal to the mean of the items 
# multiplied by the number of items, i.e., the prorated score, unless
# the number of missing items (i.e., NA) exceeds min_num_items, then
# the function returns NA.

score_a_survey <- function (items, min_num_items = length(items)) {
  # Handle some errors
  if (!is.vector(items)) {
    stop("This function only handles single vectors of numeric survey data. Try again with items as a vector.")
  }
  if (min_num_items > length(items)) {
    stop("The argument min_num_items is larger than the number of items.")
  }
  if (min_num_items <= 0) {
    stop("The argument min_num_items should not be zero or negative.")
  }
  # Done handling errors
  # Next, calculate the score as the prorated sum if enough items are present,
  # otherwise set score to NA
  
  ifelse(sum(!is.na(items)) >= min_num_items,
         length(items) * mean(items, na.rm = TRUE),
         NA)
}

# Percent-of-Maximum-Possible (POMP) Scores -------------------------------
# This function takes a single vector of survey scores as input.
# It returns score re-scaled to 0-100 percent metric.
# Thus, the minimum possible score is 0%,
# the maximum possible score is 100%, and other scores in between are possible
#
# Arguments:  raw_scores - A vector containing numeric survey scores
#             min_possible - The minimum possible score on the survey
#             max_possible - The maximum possible score on the survey
# Returns: A vector of survey scores normalized to 0-100%
# This function is based on the work of Patricia Cohen et al. (1999)
# Cohen, P., Cohen, J., Aiken, L. S., & West, S. G. (1999).
# The problem of units and the circumstance for POMP.
# Multivariate behavioral research, 34(3), 315-346.

pomp <- function(raw_scores, min_possible, max_possible) {
  # Handle some errors
    if(!is.vector(raw_scores) || !is.numeric(raw_scores)) {
    stop("raw_scores must be a vector of numeric data. Please try again")
  }
  if(min_possible >= max_possible) {
    stop("min_possible must be less than max_possible")
  }
  if(min(raw_scores, na.rm = TRUE) < min_possible) {
    stop("Some elements of raw_scores are less than min_possible.")    
  }
  if(max(raw_scores, na.rm = TRUE) > max_possible) {
    stop("Some elements of raw_scores are more than max_possible.")    
  }
  # Done handling errors. Now calculate and return POMP scores:
  (raw_scores - min_possible) / (max_possible - min_possible) * 100
}
