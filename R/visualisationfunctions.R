

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


