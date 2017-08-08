
#' Summary statistics for numerical variables
#' @description This function will return summary statistics (missing values,
#' mean, SD, SE, median, min, max, range, skew and kurtosis) for a specified numerical variable
#' @param x = variable
#' @return vector with summary statistics
#' @export

SummaryStatsNum <- function(x) {

  if (!is.numeric(x)){
    stop ("x is not a vector of type: numeric", call. = FALSE)
  }

  missingvalues <- sum(is.na(x))
  x <- na.omit(x)
  mean <- mean(x)
  sd <- sd(x)
  se <- sd(x)/sqrt(length(x))
  median <- median(x)
  min <- min(x)
  max <- max(x)
  range <- range(x)
  skew <- psych::skew(x)
  kurtosis <- psych::kurtosi(x)

  c(missingvalues = missingvalues, mean = mean, sd = sd, se = se, median = median, min = min,
    max = max, range = range, skew = skew, kurtosis = kurtosis)
}

#' Summary statistics for categorical variables
#' @description This function will return summary statistics (counts, percentage, missing values) for a specified categorical variable
#' @param x = variable
#' @return Data frame with summary statistics (count and percentage) for each level of the categorical variable
#' @export

SummaryStatsCat <- function(x){

  if (!is.factor(x)){
    stop ("x is not a vector of type: factor", call. = FALSE)
  }
    count <- summary(x)
    sum <- sum(count)
    percentage <- (count/sum) * 100

    data.frame(count, percentage)
}
