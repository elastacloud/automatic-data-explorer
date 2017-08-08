

#' Create a histogram of a target variable
#' @description Standard evaluation version of autoHistogramPlot.
#' Generates either a static \code{ggplot} or interactive \code{plotly}
#' histogram visualisation of the chosen target variable.
#' @param df Dataframe that contains the target variable
#' @param target String giving the name of the target variable
#' @param binwidth The width of the bins. The default \code{NULL} gives
#' standard \code{geom_histogram} behaviour, but should be overridden with
#' your own value
#' @param interactiveplot If \code{FALSE}, the default, returns a ggplot
#' visualisation of the histogram of the target variable. If \code{TRUE},
#' returns an interactive plotly visualisation of the histogram.
#' @param xlabel Provide a character to override the default label for the
#' x axis
#' @param ... Other arguments passed onto \code{geom_histogram}, such as
#' \code{colour = "blue"} or \code{fill = NA}
#' @param stat Defaults to \code{stat = "bin"} for numeric data. If categorical data
#' is passed then the function will automatically change to \code{stat = "count"}
#' @return A histogram of the target variable from the provided data
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_histogram xlab
autoHistogramPlot_ <- function(df, target,
                              binwidth = NULL, interactiveplot = FALSE,
                              xlabel = NULL, ..., stat = "bin") {

  # If none numeric data is being used change to stat = 'count'
  if (is.character(df[[target]]) | is.factor(df[[target]])) {
    stat <- "count"
    message("Using stat = 'count'")
  }

  # The ... argument allows the user to collect arguments to call another function
  outplot <- ggplot(df, aes_string(target), environment = environment()) +
    geom_histogram(binwidth = binwidth, stat = stat, ...) +
    xlab(ifelse(is.null(xlabel), target, xlabel))

  if (interactiveplot) {
    plotly::ggplotly(outplot)
  } else {
    outplot
  }
}





#' Create a densiy estimate of a target variable
#' @description Standard evaluation version of autoDensityPlot. Generates either
#'  a static \code{ggplot} or interactive \code{plotly} density estimate
#'  visualisation of the chosen target variable using \code{geom_density}.
#' @param df Dataframe that contains the target variable
#' @param target String giving the name of the target variable
#' @param interactiveplot If \code{FALSE}, the default, returns a \code{ggplot2}
#' visualisation of the density estimate of the target variable. If
#' \code{TRUE}, returns an interactive \code{plotly} visualisation of the histogram.
#' @param xlabel Provide a character to override the default label for the
#' x axis
#' @param ... Other arguments passed onto \code{geom_density}, such as
#' \code{colour = "red"} or \code{size = 2}
#' @return A density estimate visualisation of the target variable from the
#' provided data
#' @export
#' @importFrom ggplot2 ggplot aes_string geom_density xlab
autoDensityPlot_ <- function(df, target,
                            interactiveplot = FALSE, xlabel = NULL, ...) {

  if (!is.numeric(df[[target]])) {
    stop("None-numeric data is not currently supported for this function",
         call. = FALSE)
  }

  outplot <- ggplot(df, aes_string(target), environment = environment()) +
    geom_density(...) +
    xlab(ifelse(is.null(xlabel), target, xlabel))

  if (interactiveplot) {
    plotly::ggplotly(outplot)
  } else {
    outplot
  }
}
