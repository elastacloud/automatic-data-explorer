
#'Multivariate outlier detection for numerical variables
#'@description This function will return a list of 2 objects (1: the original dataframe with an appended column identifying outliers (outliers==0), 2: A plot of the first two components of PCA, outliers identified in red)
#'@param DF = dataframe
#'@return Returns a list of 2 objects (1: the original dataframe with an appended column identifying outliers (outliers==0), 2: A plot of the first two components of PCA, outliers identified in red)

MultivariateOutlier <- function(DF){

  for(i in 1:ncol(DF)){
    DF[is.na(DF[,i]), i] <- mean(DF[,i], na.rm = TRUE)
  }

  # Compute PCA on DF
  pca <- princomp(DF,scores = T)
  pca
  summary(pca)

  pca.scores=as.data.frame(pca$scores)
  pca.scores1 <- pca.scores[,1:2]
  ggplot2::qplot(Comp.1,Comp.2,data=pca.scores1)

  # Scaled distance matrix
  dist.Mat <- dist(scale(DF), method = "euclidean")
  hist <- hist(dist.Mat,col = "red",main = "Histogram of Distance Matrix",xlab="Distance")

  #Find the optimal epsilon value for dbscan
  hist1 <- list(hist$breaks, hist$counts)
  hist1max <- max(unlist(lapply(hist1,length)))
  hist1 <- lapply(hist1,function(x) c(x, rep(NA,hist1max-length(x))))
  hist1 <- data.frame(hist1)
  colnames(hist1) <- c("Distance","Frequency")
  epsilon <- hist1[which.max(hist1$Frequency), 1]

  #Find the optimum minimum points value for dbscan
  mat=as.matrix(dist.Mat)
  mat.long=reshape2::melt(mat)
  mat.long$flag=mat.long$value>0.0 & mat.long$value<=epsilon
  d <- dplyr::summarise(dplyr::group_by(mat.long,Var1),count=sum(flag))
  hist2 <-hist(d$count,col="red",main="Histogram of # of minmum points", xlab="# of minimum points")

  hist3 <- list(hist2$breaks, hist$counts)
  hist3max <- max(unlist(lapply(hist3,length)))
  hist3 <- lapply(hist3,function(x) c(x, rep(NA, hist3max-length(x))))
  hist3 <- data.frame(hist3)
  colnames(hist3) <- c("Distance", "Frequency")
  MinimumPts <- hist3[which.max(hist3$Frequency), 1]

  dbscan.fit <- fpc::dbscan(DF,eps = epsilon ,MinPts = MinimumPts,scale = T,method = "raw")
  OutliersPlot <- ggplot2::qplot(Comp.1,Comp.2,data=pca.scores1)+ggplot2::geom_point(col=dbscan.fit$cluster+2)

  Outliers <- data.frame(dbscan.fit$cluster)
  DFOutliers <- cbind(DF, Outliers)
  names(DFOutliers)[names(DFOutliers)== "dbscan.fit.cluster"] <- "Outliers"

  Outliers <- list(DFOutliers,OutliersPlot)
  names(Outliers) <- c("DFOutliers", "OutliersPlot")
  Outliers

}
