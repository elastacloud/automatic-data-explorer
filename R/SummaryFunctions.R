
#' Summary statistics for numerical variables
#' @description This function will return summary statistics (missing values,
#' mean, SD, SE, median, min, max, range, skew and kurtosis) for a specified numerical variable
#' @param x = variable
#' @return vector with summary statistics


SummaryStatsNum <- function(x) {

  if (!is.numeric(x)){
    stop ("x is not a vector of type: numeric", call. = FALSE)
  }

  missingvalues <- sum(is.na(x))
  x <- stats::na.omit(x)
  mean <- mean(x)
  sd <- sd(x)
  se <- sd(x)/sqrt(length(x))
  median <- median(x)
  min <- min(x)
  max <- max(x)
  range <- max -  min
  skew <- psych::skew(x)
  kurtosis <- psych::kurtosi(x)

  c(missingvalues = missingvalues, mean = mean, sd = sd, se = se, median = median, min = min,
    max = max, range = range, skew = skew, kurtosis = kurtosis)
}

#' Summary statistics for categorical variables
#' @description This function will return summary statistics (counts, percentage, missing values) for a specified categorical variable
#' @param x = variable
#' @return Data frame with summary statistics (count and percentage) for each level of the categorical variable


SummaryStatsCat <- function(x){

  if (!is.factor(x)){
    stop ("x is not a vector of type: factor", call. = FALSE)
  }
  count <- summary(x)
  sum <- sum(count)
  percentage <- (count/sum) * 100

  data.frame(count, percentage)
}

#' Summary function for both categorical and numerical variables
#' @description This function will return relevant summary statistics dependent on the data type of the variable inputted
#' @param x A variable of type categorical or numerical
#' @return Data frame with relevant summary statistics

VSummary <- function(x){
  if (is.numeric(x)){
    SummaryNum <- data.frame(SummaryStatsNum(x))
  } else{
    if (is.factor(x)){
      SummaryCat <- data.frame(SummaryStatsCat(x))
    } else {
      stop("x is not a data type numeric or factor", call. = FALSE)
    }
  }
}

#' Summary function that takes in a data frame or vector of categorical or numerical varibales and provides summary statistics
#' @description This function will provide relevant summary statistics for inputted data frames or vectors of categorrical or numerical variables
#' @param object can be a data frame or vector
#' @return Data frame with relevant summary statistics
#' @export

Summary <- function(object){
  if (class(object) == "data.frame"){
    lapply(object, VSummary)
  } else{
    data.frame(VSummary(object))
  }
}
