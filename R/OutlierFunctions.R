

#'Outlier detection for numerical variables
#'@description This function will return a list of 4 objects (1: a data frame with mild outliers and their indexes, 2: a data frame with extreme outliers and their indexes, 3: a vector with mild outlier thresholds, 4: a vector with extreme outlier thresholds)
#'@param x = variable
#'@return Returns a list with 4 objects showing mild and extreme outliers and the thresholds for each.


Outliers <- function(x){
  x <- na.omit(x)
  lowerq <- quantile(x)[2]
  upperq <- quantile(x)[4]
  iqr <- upperq - lowerq
  #mild outliers:
  mild.threshold.upper <- (iqr *1.5) + upperq
  mild.threshold.lower <- lowerq - (iqr * 1.5)
  Mild.thresholds <- c(mild.threshold.lower,mild.threshold.upper)
  #extreme outliers:
  extreme.threshold.upper <- (iqr * 3) + upperq
  extreme.threshold.lower <- lowerq - (iqr * 3)
  Extreme.thresholds <- c(extreme.threshold.lower,extreme.threshold.upper)
  #calculate which values are potential outliers:
  M.index <- which(x > mild.threshold.upper | x < mild.threshold.lower)
  M.outliers <- x[M.index]
  MildOutliers <- data.frame(Index = M.index, mild.outliers = M.outliers)

  E.index <- which( x > extreme.threshold.upper | x < extreme.threshold.lower)
  E.outliers <- x[E.index]
  ExtremeOutliers <- data.frame(Index = E.index, extreme.outliers = E.outliers)

  Outliers <- list(MildOutliers, ExtremeOutliers, Mild.thresholds, Extreme.thresholds)
  names(Outliers) <- c("MildOutliers", "ExtremeOutliers", "MildThresholds", "ExtremeThresholds")

  if (NROW(Outliers$ExtremeOutliers) == 0){
    stop("There are no outliers in this variable", call. = FALSE)
  }else{
    return(Outliers)
  }
}

#'Percentage of observations which are mild and extreme outliers
#'@description This function will calculate what percentage of observations are mild and extreme outliers
#'@param x = variable
#'@return A data frame with the percentage of observations which are mild and extreme outliers

OutlierPercentage <- function(x){
  outliers <- Outliers(x)
  #calculate percentage of obs that are outliers:

  PercentageMildOutliers <- (sum(!is.na(outliers$MildOutlier[,2]))/length(x))*100
  PercentageExtremeOutliers <- (sum(!is.na(outliers$ExtremeOutliers[,2]))/length(x))*100

  OutlierPercentage <- data.frame(PercentageMildOutliers, PercentageExtremeOutliers)

  return(OutlierPercentage)
  }


#' Mean with and without mild and extreme outliers
#' @description This function will compute the mean of x with and without the mild and extreme outliers
#' @param x = variable
#' @return A data frame with the mean without any outliers removed, the mean with mild and extreme outliers removed

OutlierMean <- function(x){
  outliers <- Outliers(x)

  MildOutliers <- outliers$MildOutliers[,2]
  ExtremeOutliers <- outliers$ExtremeOutliers[,2]

  x = na.omit(x)
  MeanWithOutliers <- mean(x)
  MeanWithoutMildOutliers <- mean(x[!x %in% MildOutliers])
  MeanWithoutExtremeOutliers <- mean(x[!x %in% ExtremeOutliers])

  OutlierMeans <- data.frame(MeanWithOutliers, MeanWithoutMildOutliers, MeanWithoutExtremeOutliers)

  return(OutlierMeans)

}
#######################################################################################################
#'Outlier detection and outlier summary statistics
#'@description This function will return a list of 6 objects detailing identified mild and extreme outliers, outlier thresholds and summary statistics for the variable with and without mild/extreme outliers
#'@param x = variable
#'@return list of 6 objects (1:mild outliers, 2: extreme outliers, 3: mild outlier thresholds, 4: extreme outlier thresholds, 5: Percentage mild and extreme outleirs, 6: variable with and  without mild and extreme outliers)


# main function that computes all outlier sub-functions
Outlier <- function(x){
  outliers <- Outliers(x)
  outlierPercentage <- OutlierPercentage(x)
  outlierMean <- OutlierMean(x)

  Outliers <- list(outliers, outlierPercentage, outlierMean)
  names(Outliers) <- c("outliers", "outlierPercentage", "outlierMean")
  return(Outliers)
}
