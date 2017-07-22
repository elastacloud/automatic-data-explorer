
#'Summary statistics for numerical variables
#'@description This function will return summary statistics (missing values,
#'mean, SD, SE, median, min, max, range, skew and kurtosis) for a specified numerical variable
#'@param x = variable
#'@return vector with summary statistics

SummaryStatsNum <- function(x){
  
  MissingValues <- sum(is.na(x))
  Mean <- mean (x)
  SD <- sd (x)
  SE <- function(x)sd(x)/sqrt(length(x))
  Median <- median (x)
  Min <- min (x)
  Max <- max (x)
  Range <- range (x)
  Skew <- psych::skew (x)
  Kurtosis <- psych::kurtosi (x)
  
  c(Missingvalues = MissingValues, Mean = Mean, SD = SD,  Median = Median, Min = Min, Max = Max, Range = Range, Skew = Skew, Kurtosis = Kurtosis)
}

#'Summary statistics for categorical variables
#'@description This function will return summary statistics (counts, percentage, missing values) for a specified categorical variable
#'@param x = variable
#'@return Data frame with summary statistics (count and percentage) for each level of the categorical variable

SummaryStatsCat <- function(x){
  if (is.factor(x) == TRUE){
    Count <- summary(x)
    Sum <- sum(Count)
    Percentage <- Count/Sum*100
    
    Summary <- data.frame(Count, Percentage)
    return(Summary)
  } else{
    print ("x is not a vector of tpye: factor")
  }
}