

#' Create a histogram of a target variable
#' @description Generates either a static ggplot or interactive plotly
#' histogram visualisation of the chosen target variable.
#' @param df Dataframe that contains the target variable
#' @param target String giving the name of the target variable
#' @param interactiveplot If \code{FALSE}, the default, returns a ggplot
#' visualisation of the histogram of the target variable. If \code{TRUE},
#' returns a interactive plotly visualisation of the histogram.
gghistogram <- function(df, target,
                        interactiveplot = FALSE){



  outplot <- ggplot(df, aes_string(target), environment = environment()) +
                      geom_histogram(colour = "black")

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
ggdensity <- function(df, target,
                      interactiveplot = FALSE){

  outplot <- ggplot(df, aes_string(target), environment = environment()) +
                      geom_density(colour = "black")

  if(interactiveplot) {

    plotly::ggplotly(outplot)

  } else {

    outplot

  }
}


