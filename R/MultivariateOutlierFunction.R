
#' Calculation of the optimal value for the epsilon parameter used in dbscan
#' @description This function calculates the optimal value for the epsilon parameter used in dbscan.
#' The epsilon parameter specifies how close points should be to each other to be considered part of a cluster.
#' @param df, a dataframe of numerical variables
#' @return This function returns the optimal value for the epsilon parameter used in dbscan based on the inputted dataframe.

OptimalEpsilon <- function(df){
  # Scaled distance matrix
  distMat <- stats::dist(scale(df), method = "euclidean")
  hist <- hist(distMat, col = "red", main = "Histogram of Distance Matrix", xlab="Distance")

  #Find the optimal epsilon value for dbscan
  hist1 <- list(hist$breaks, hist$counts)
  hist1max <- max(unlist(lapply(hist1, length)))
  hist1 <- lapply(hist1,function(x) c(x, rep(NA, hist1max-length(x))))
  hist1 <- data.frame(hist1)
  colnames(hist1) <- c("Distance","Frequency")
  epsilon <- hist1[which.max(hist1$Frequency), 1]
  epsilon
}

#' Calculation of the optimal value for the minimum points parameter used in dbscan
#' @description This function calculates the optimum value for the minimum points parameter in dbscan, based on the optimal epsilon value.
#' The minimum points parameter specifies how many neighbours a point should have to be included in a cluster.
#' @param df, a dataframe of numerical variables
#' @return This function returns a list of the optimal values for the minimum points and epsilon parameters based on the inputted dataframe.

OptimalMinPoints <- function(df){
  epsilon <- OptimalEpsilon(df)

  distMat <- as.matrix(stats::dist(scale(df), method = "euclidean"))
  hist <- hist(distMat, col = "red", main = "Histogram of Distance Matrix", xlab="Distance")

  matLong <- reshape2::melt(distMat)
  matLong$flag <- matLong$value > 0.0 & matLong$value <= epsilon
  d <- dplyr::summarise_(dplyr::group_by_(matLong, "Var1"), count = "sum(flag)")
  hist2 <- hist(d$count, col="red", main="Histogram of # of minmum points", xlab="# of minimum points")

  hist3 <- list(hist2$breaks, hist$counts)
  hist3max <- max(unlist(lapply(hist3, length)))
  hist3 <- data.frame(lapply(hist3,function(x) c(x, rep(NA, hist3max-length(x)))))
  colnames(hist3) <- c("Distance", "Frequency")
  minimumPts <- hist3[which.max(hist3$Frequency), 1]

  optimalParameters <- list(minimumPts, epsilon)
  names(optimalParameters) <- c("minPts", "epsilon")

  optimalParameters
}


#' Multivariate outlier detection for numerical variables
#' @description This function will return a list of 2 objects (1: the original dataframe with an appended column identifying outliers (outliers==0), 2: A plot of the first two components of PCA, outliers identified in red)
#' @param df, dataframe of numerical variables
#' @return Returns a list of 2 objects (1: the original dataframe with an appended column identifying outliers (outliers==0), 2: A plot of the first two components of PCA, outliers identified in red)
#' @export

MultivariateOutlier <- function(df){

  for (i in seq_along(df)){
    if(!is.numeric(df[[i]])){
      stop ("df is not a vector of type: numeric", call. = FALSE)
    }

  }

  #replace all NAs with the mean of the column
  for(i in seq_along(df)){
    df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)
  }

  #calculate optimal epsilon and minimum points parameters based on df
  optimalParmeters <- OptimalMinPoints(df)
  minPts <- optimalParmeters$minPts
  epsilon <- optimalParmeters$epsilon

  # Compute PCA on DF
  pca <- stats::princomp(df,scores = T)

  pcaScores <- data.frame(pca$scores)
  pcaScores1 <- pcaScores[,1:2]

  # use dbscan to identify outliers
  dbscanFit <- fpc::dbscan(df,eps = epsilon, MinPts = minPts,scale = T,method = "raw")
  outliersPlot <- ggplot2::ggplot(pcaScores1, aes_string("Comp.1", "Comp.2")) + ggplot2::geom_point(col= dbscanFit$cluster+2)
 # outliersPlot <- ggplot2::qplot(Comp.1, Comp.2, data = pcaScores1) + ggplot2::geom_point(col= dbscanFit$cluster+2)

  outliers <- data.frame(dbscanFit$cluster)
  dfOutliers <- cbind(df, outliers)
  names(dfOutliers)[names(dfOutliers) == "dbscanFit.cluster"] <- "outliers"

  Outliers <- list(dfOutliers,outliersPlot)
  names(Outliers) <- c("dfOutliers", "OutliersPlot")
  Outliers
}
