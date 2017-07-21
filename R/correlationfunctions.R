
#' Correlations between a target variable and all other variables in a data frame
#' @description This function will return the correlation between a specfied target
#' variable and all other variables in the same data frame. Uses \code{cor}
#' to calculate correlations.

targetCorrelations <- function(df, target,
                               use = "everything",
                               method = c("pearson", "kendall", "spearman")) {

  varnames <- colnames(df)[colnames(df) != target] # Get column names that are not the 'target'

  # purrr provides type consistent functions similar to the apply family of functions
  corrs <- purrr::map_dbl(df[varnames], ~ cor(., df[target], method = method, use = use))

  corrs[order(abs(corrs), decreasing = TRUE)]  # Return ordered correlations

  # TO DO: Allow user to select top N correlations to return

}
