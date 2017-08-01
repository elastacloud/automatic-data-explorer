

#' Create a histogram of a target variable
#' @description Generates either a static ggplot or interactive plotly
#' histogram visualisation of the chosen target variable.
#' @param df Dataframe that contains the target variable
#' @param target String giving the name of the target variable
#' @param binwidth The width of the bins. The default \code{NULL} gives
#' standard \code{ggplot2::geom_histogram} but should be overridden with
#' your own value
#' @param interactiveplot If \code{FALSE}, the default, returns a ggplot
#' visualisation of the histogram of the target variable. If \code{TRUE},
#' returns a interactive plotly visualisation of the histogram.
#' @param xlabel Provide a character to override the default label for the
#' x axis
#' @param ... Other arguments passed onto \code{ggplot2::geom_histogram}, such as
#' \code{colour = "blue"} or \code{fill = NA}
#' @param stat Defaults to \code{stat = "bin"} for numeric data. If categorical data
#' is passed then the function will automatically change to \code{stat = "count"}
#' @return A histogram of the target variable from the provided data
autoHistogramPlot <- function(df, target,
                              binwidth = NULL, interactiveplot = FALSE,
                              xlabel = NULL, ..., stat = "bin") {

  target <- substitute(target)

  if(is.symbol(target)) {
    target <- deparse(target)
  }

  # If none numeric data is being used change to stat = 'count'
  if (is.character(df[[target]]) | is.factor(df[[target]])) {
    stat <- "count"
    message("Using stat = 'count'")
  }

  # The ... argument allows the user to collect arguments to call another function
  outplot <- ggplot(df, aes_string(target), environment = environment()) +
                      geom_histogram(binwidth = binwidth, stat = stat, ...) +
                      xlab(ifelse(is.null(xlabel), target, xlabel))

  if(interactiveplot) {

    plotly::ggplotly(outplot)

  } else {

    outplot

  }
}

#' Create a densiy estimate of a target variable
#' @description Generates either a static ggplot or interactive plotly
#' density estimate visualisation of the chosen target variable.
#' @param df Dataframe that contains the target variable
#' @param target String giving the name of the target variable
#' @param interactiveplot If \code{FALSE}, the default, returns a ggplot
#' visualisation of the density estimate of the target variable. If
#' \code{TRUE}, returns a interactive plotly visualisation of the histogram.
autoDensityPlot <- function(df, target,
                      interactiveplot = FALSE) {

  target <- substitute(target)

  if(is.symbol(target)) {
    target <- deparse(target)
  }

  outplot <- ggplot(df, aes_string(target), environment = environment()) +
                      geom_density(colour = "black")

  if(interactiveplot) {

    plotly::ggplotly(outplot)

  } else {

    outplot

  }
}


#' Plot a correlation matrix
#' @description Given a correlation matrix this function will generate a
#' correlation plot. Uses \code{corrplot::corrplot} for non-interactive plotting
#' and \code{heatmaply::heatmaply} for interactive plotting
#' @param m Correlation matrix to plot
#' @param cluster If \code{cluster = TRUE} then the correlation matrix will be
#' reordered using the \code{"hclust"} method in \code{corrplot::corrMatOrder}
#' @param interactiveplot If \code{FALSE}, the default, returns a \code{corrplot}
#' visualisation of the correlation matrix. If \code{TRUE}, returns a interactive
#' \code{heatmaply} visualisation of the correlation matrix
#' @return Correlation plot of the provided correlation matrix
autoCorrelationPlot <- function(m,
                                cluster = FALSE, interactiveplot = FALSE) {

  # Check that m is a matrix
  if (!is.matrix(m)) {
    stop("`m` is of class ", class(m),"; it must be a matrix")
  }

  # Check that the passed matrix is a correlation matrix
  if (any(abs(m) > 1) | !isSymmetric(m)) {
    stop("`m` does not appear to be a correlation matrix:",
         ifelse(any(abs(m) > 1), " all elements ![-1, 1]",
                                 " matrix is not symmetrical"))
  }

  # Reorder correlation matrix based on hclust of correlations
  if(cluster) {
    newidxs <- corrplot::corrMatOrder(m, order = "hclust")
    m <- m[newidxs, newidxs]
  }

  if(cluster) {  # Use reordered matrix for the plot
    if(interactiveplot) {
      heatmaply::heatmaply(m, Colv = F, Rowv = F , limits = c(-1, 1))
    } else {
      corrplot::corrplot(m)
    }
  } else {  # Otherwise, don't
    if(interactiveplot) {
      heatmaply::heatmaply(m, Colv = F, Rowv = F , limits = c(-1, 1))
    } else {
      corrplot::corrplot(m)
    }
  }
}



