
#'Outlier detection and outlier summary statistics
#'@description This function will return identified mild and extreme outliers, provide a boxplot of the variable and summary statistics for the variable with and without mild/extreme outliers
#'@param x = variable
#'@return A boxplot and list of data frames (1: Values of mild and extreme outliers, 2: The percentage of obseervations that are mild and extreme outliers, 3: The mean of x with and without mild and extreme outliers) )

# main function that computes all outlier sub-functions 
Outlier <- function(x){
  outliers <- Outliers(x)
  outlierPercentage <- OutlierPercentage(x)
  outlierMean <- OutlierMean(x)
  
  boxplot(x, Main = "Potential Outliers")
  
  Outliers <- list(outliers, outlierPercentage, outlierMean)
  return(Outliers)
}

# function to extract the potential mild and extremem outliers

Outliers <- function(x){
  lowerq = quantile(x)[2]
  upperq = quantile(x)[4]
  iqr = upperq - lowerq
  #mild outliers:
  mild.threshold.upper = (iqr *1.5) + upperq
  mild.threshold.lower = lowerq - (iqr * 1.5)
  #extreme outliers:
  extreme.threshold.upper = (iqr * 3) + upperq
  extreme.threshold.lower = lowerq - (iqr * 3)
  #calculate which values are potential outliers:
  MildOutliers <- subset(x, x > mild.threshold.upper | x < mild.threshold.lower)
  ExtremeOutliers <- subset(x, x > extreme.threshold.upper | x < extreme.threshold.lower)
  Outliers <- data.frame(MildOutliers, ExtremeOutliers)
  
  return(Outliers)
}

#-------#

#function to calculate the percentage of dataset that are potential mild and extreme outliers

OutlierPercentage <- function(x){
  Outliers <- Outliers(x)
  #calculate percentage of obs that are outliers:
  PercentageMildOutliers <- (sum(!is.na(Outliers[,1]))/length(x))*100
  PercentageExtremeOutliers <- (sum(!is.na(Outliers[,2]))/length(x))*100
  
  OutlierPercentage <- data.frame(PercentageMildOutliers, PercentageExtremeOutliers)
  
  return(OutlierPercentage)
  
}

#-------#

# function to calculate the mean with and without outliers

OutlierMean <- function(x){
  outliers <- Outliers(x)
  MildOutliers <- outliers[,1]
  ExtremeOutliers <- outliers[,2]
  
  MeanWithOutliers <- mean(x)
  MeanWithoutMildOutliers <- mean(x[!x %in% MildOutliers])
  MeanWithoutExtremeOutliers <- mean(x[!x %in% ExtremeOutliers])
  
  OutlierMeans <- data.frame(MeanWithOutliers, MeanWithoutMildOutliers, MeanWithoutExtremeOutliers)
  
  return(OutlierMeans) 
  
}





