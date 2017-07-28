

#'Outlier detection for numerical variables
#'@description This function will return a data frame with the mild and extreme outliers if outliers are present
#'@param x = variable
#'@return A data frame with mild and extreme outliers if outliers are present

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
  #if (outliers != "There are no outliers in this variable"){

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
#'@description This function will return identified mild and extreme outliers, provide a boxplot of the variable and summary statistics for the variable with and without mild/extreme outliers
#'@param x = variable
#'@return A boxplot and list of data frames (1: Values of mild and extreme outliers, 2: The percentage of obseervations that are mild and extreme outliers, 3: The mean of x with and without mild and extreme outliers) )

# main function that computes all outlier sub-functions
Outlier <- function(x){
  outliers <- Outliers(x)
  outlierPercentage <- OutlierPercentage(x)
  outlierMean <- OutlierMean(x)

  Outliers <- list(outliers, outlierPercentage, outlierMean)
  names(Outliers) <- c("outliers", "outlierPercentage", "outlierMean")
  return(Outliers)
}
