

#'Outlier detection for numerical variables
#'@description This function will return a list of 4 objects (1: a data frame with mild outliers and their indexes, 2: a data frame with extreme outliers and their indexes, 3: a vector with mild outlier thresholds, 4: a vector with extreme outlier thresholds)
#'@param x variable of interest
#'@return Returns a list with 4 objects showing mild and extreme outliers and the thresholds for each.


OutlierValues <- function(x){
  x <- na.omit(x)
  lowerq <- quantile(x)[2]
  upperq <- quantile(x)[4]
  iqr <- upperq - lowerq
  #mild outliers:
  mildThresholdUpper <- (iqr *1.5) + upperq
  mildThresholdLower <- lowerq - (iqr * 1.5)
  mildThresholds <- c(mildThresholdLower,mildThresholdUpper)
  #extreme outliers:
  extremeThresholdUpper <- (iqr * 3) + upperq
  extremeThresholdLower <- lowerq - (iqr * 3)
  extremeThresholds <- c(extremeThresholdLower,extremeThresholdUpper)
  #calculate which values are potential outliers:
  mIndex <- which(x > mildThresholdUpper | x < mildThresholdLower)
  mOutliers <- x[mIndex]
  mildOutliers <- data.frame(Index = mIndex, mildOutliers = mOutliers)

  eIndex <- which( x > extremeThresholdUpper | x < extremeThresholdLower)
  eOutliers <- x[eIndex]
  extremeOutliers <- data.frame(Index = eIndex, extremeOutliers = eOutliers)

  outlierValueResult <- list(mildOutliers, extremeOutliers, mildThresholds, extremeThresholds)
  names(outlierValueResult) <- c("mildOutliers", "extremeOutliers", "mildThresholds", "extremeThresholds")

  if (NROW(outlierValueResult$extremeOutliers) == 0 | NROW(outlierValueResult$mildOutliers) == 0){
    stop("There are no outliers in this variable", call. = FALSE)
  }else{
    return(outlierValueResult)
  }
}

#'Percentage of observations which are mild and extreme outliers
#'@description This function will calculate what percentage of observations are mild and extreme outliers
#'@param outliers, The list output from the OutlierValues function, x, the variable of interest
#'@return A data frame with the percentage of observations which are mild and extreme outliers

OutlierPercentage <- function(outliers, x){

  percentageMildOutliers <- (sum(!is.na(outliers$mildOutliers[,2]))/length(x))*100
  percentageExtremeOutliers <- (sum(!is.na(outliers$extremeOutliers[,2]))/length(x))*100

  return(data.frame(percentageMildOutliers, percentageExtremeOutliers))
}


#' Mean with and without mild and extreme outliers
#' @description This function will compute the mean of x with and without the mild and extreme outliers
#' @param outliers, the list output from the OutlierValues function, x, the variable of interest
#' @return A data frame with the mean without any outliers removed, the mean with mild and extreme outliers removed

OutlierMean <- function(outliers, x){

  mildOutliers <- outliers$mildOutliers[,2]
  extremeOutliers <- outliers$extremeOutliers[,2]

  x <- na.omit(x)
  meanWithOutliers <- mean(x)
  meanWithoutMildOutliers <- mean(x[!x %in% mildOutliers])
  meanWithoutExtremeOutliers <- mean(x[!x %in% extremeOutliers])

  return(data.frame(meanWithOutliers, meanWithoutMildOutliers, meanWithoutExtremeOutliers))
}
#######################################################################################################
#'Outlier detection and outlier summary statistics
#'@description This function will return a list of 6 objects detailing identified mild and extreme outliers, outlier thresholds and summary statistics for the variable with and without mild/extreme outliers
#'@param x, the variable of interest
#'@return list of 6 objects (1:mild outliers, 2: extreme outliers, 3: mild outlier thresholds, 4: extreme outlier thresholds, 5: Percentage mild and extreme outleirs, 6: variable with and  without mild and extreme outliers)
#'@export

# main function that computes all outlier sub-functions
Outlier <- function(x){
  outlierValues <- OutlierValues(x)
  outlierPercentage <- OutlierPercentage(outlierValues,x)
  outlierMean <- OutlierMean(outlierValues, x)

  Outliers <- list(outliers, outlierPercentage, outlierMean)
  names(Outliers) <- c("outliers", "outlierPercentage", "outlierMean")
  return(Outliers)
}
