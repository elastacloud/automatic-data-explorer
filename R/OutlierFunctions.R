

OutlierValues <- function(x){
  x <- stats::na.omit(x)
  lowerq <- stats::quantile(x)[2]
  upperq <- stats::quantile(x)[4]
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
    outlierValueResult
  }
}



OutlierPercentage <- function(outliers, x){

  percentageMildOutliers <- (sum(!is.na(outliers$mildOutliers[,2])) / length(x)) * 100
  percentageExtremeOutliers <- (sum(!is.na(outliers$extremeOutliers[,2])) / length(x)) * 100

  data.frame(percentageMildOutliers, percentageExtremeOutliers)
}


OutlierMean <- function(outliers, x){

  mildOutliers <- outliers$mildOutliers[,2]
  extremeOutliers <- outliers$extremeOutliers[,2]

  x <- na.omit(x)
  meanWithOutliers <- mean(x)
  meanWithoutMildOutliers <- mean(x[!x %in% mildOutliers])
  meanWithoutExtremeOutliers <- mean(x[!x %in% extremeOutliers])

  data.frame(meanWithOutliers, meanWithoutMildOutliers, meanWithoutExtremeOutliers)
}
#######################################################################################################
#' Outlier detection and outlier summary statistics
#' @description This function will return a list of 6 objects detailing identified mild and extreme outliers, outlier thresholds and summary statistics for the variable with and without mild/extreme outliers
#' @param x the variable of interest
#' @return list of 6 objects (1:mild outliers, 2: extreme outliers, 3: mild outlier thresholds, 4: extreme outlier thresholds, 5: Percentage mild and extreme outleirs, 6: variable with and  without mild and extreme outliers)
#' @export

Outlier <- function(x){

  if (!is.numeric(x)){
    stop ("x is not a vector of type: numeric", call. = FALSE)
  }
  outlierValues <- OutlierValues(x)
  outlierPercentage <- OutlierPercentage(outlierValues,x)
  outlierMean <- OutlierMean(outlierValues, x)

  Outliers <- list(outlierValues, outlierPercentage, outlierMean)
  names(Outliers) <- c("outliers", "outlierPercentage", "outlierMean")
  Outliers
}
