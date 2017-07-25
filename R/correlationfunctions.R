
#' Correlations between a target variable and all other variables in a data frame
#' @description This function will return the correlation between a specfied target
#' variable and all other variables in the same data frame. Uses \code{cor}
#' to calculate correlations.
#' @param df Dataframe containing all variables
#' @param target String giving the name of the target variable
#' @param N Default \code{NULL} will return all correlations. Change to e.g. \code{N = 5}
#' to return only the 5 highest absolute correlations
#' @param use Same as \code{use} in the \code{cor} function
#' @param method Same as \code{method} in the \code{cor} function
#' @return Named vector of the correlations, ordered by absolute highest to lowest.

targetCorrelations <- function(df, target,
                               N = NULL,
                               use = "everything",
                               method = c("pearson", "kendall", "spearman")) {

  # catch non-numeric `target` error
  if (!is.numeric(df[[target]])) {
    stop("`target` should be a numeric vector", call. = FALSE)
  }

  # purrr provides type consistent functions similar to the apply family of functions
  df <- df[purrr::map_lgl(df, is.numeric)]  # Ignore non-numeric columns

  varnames <- colnames(df)[colnames(df) != target]  # Get column names that are not the 'target'

  corrs <- purrr::map_dbl(df[varnames], ~ stats::cor(., df[target], method = method, use = use))

  corrs <- corrs[order(abs(corrs), decreasing = TRUE)]

  if (is.null(N)) {
    corrs  # Return all ordered correlations
  } else {
    if (N > length(corrs)) {
      warning("N is greater than number of correlations")
      corrs
    } else {
      corrs[1:N]  # Return the top N variables with highest absolute correlation
    }
  }
}



#' Calculate correlations between numeric variables
#' @description Can return one of two objects. 1. Dataframe of ranked
#' absolute correlation pairs, or 2. Correlation matrix of all variables.
#' @param df Dataframe containing all variables.
#' @param dropnotnumeric Logical, defaults to \code{TRUE}. Automatically drop any non-numeric variables
#' from the dataframe.
#' @param output Determines which output type to return. One of \code{ranked} for ranked correlation
#' pairs or \code{matrix} for the correlation matrix.
#' @param N Only used when \code{output = "ranked"}. Default \code{NULL} will return all variable pair
#' correlations. Change to e.g. \code{N = 5} to return only the 5 highest absolute correlations
#' @param use Same as \code{use} in the \code{cor} function.
#' @param method Same as \code{method} in the \code{cor} function.
#' @return Returns the correlation matrix if \code{output = "matrix"} or dataframe of ordered
#' variable pair correlations if \code{output = "ranked"}.
multivariateCorrelation <- function(df,
                                    dropnotnumeric = TRUE,
                                    output = "ranked",
                                    N = NULL,
                                    use = "everything",
                                    method = c("pearson", "kendall", "spearman")) {

  if (!output %in% c("ranked", "matrix")) {
    stop("invalid `output` argument: ", output, call. = FALSE)
  }

  # Drop all non-numeric columns if dropnotnumeric = TRUE
  if (dropnotnumeric) {
    df <- df[purrr::map_lgl(df, is.numeric)]
  }

  # Error if one of the variables is not numeric
  if (any(!purrr::map_lgl(df, is.numeric))){
    stop("non-numeric variables detected", call. = FALSE)
  }

  corrmatrix <- stats::cor(df, use = use, method = method) # Calculate correlation matrix

  if (output == "matrix"){
    corrmatrix
  } else {
    rankCorrelations(corrmatrix, autoname = FALSE, N = N)
  }
}



#' Ranks variable pair correlations
#' @description Takes in a correlation matrix and returns an ordered
#' dataframe of variable pairs. The order is from the absolute highest
#' to lowest correlation.
#' @param m A correlation matrix
#' @param N Default \code{NULL} will return all correlations. Change to e.g. \code{N = 5}
#' to return only the 5 highest absolute correlations
#' @param autoname Automatically rename matrix columns and rows; default
#' is \code{FALSE}. Only recommended if \code{m} does not have names already.
#' @return Dataframe with three columns; 1. variable 1, 2. variable 2 and
#' 3. correlation
rankCorrelations <- function(m,
                             N = NULL,
                             autoname = FALSE) {

  if (!is.matrix(m)){
    stop("Argument `m` is not a matrix", call. = FALSE)
  }

  if (autoname){
    rownames(m) <- as.character(paste0("Var", seq(1, nrow(m), 1)))
  }

  if (is.null(rownames(m))){
    stop("Rows and columns of correlation matrix must be named, consider `autoname = TRUE`",
         call. = FALSE)
  }

  ut <- upper.tri(m)

  # Create dataframe of variable pair names and correlations
  corrs <- data.frame(varOne = rownames(m)[row(m)[ut]],
                      varTwo = rownames(m)[col(m)[ut]],
                      cor = t(m)[ut])

  # order dataframe by highest to lowest absolute correlation
  corrs <- corrs[order(abs(corrs$cor), decreasing = T), ]

  if (is.null(N)) {
    corrs  # Return all ordered correlations
  } else {
    if (N > nrow(corrs)) {
      warning("N is greater than number of correlations")
      corrs
    } else {
      corrs[1:N, ]  # Return the top N variables with highest absolute correlation
    }
  }
}
