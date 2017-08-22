
variableSummary <- function(df, x) {

  if(is.numeric(df[[x]])) {
    xtype <- "numeric"
    xlength <- length(df[[x]])
    xsummary <- SummaryStatsNum(df[[x]])
    xcorrelations <- targetCorrelations(df = df, target = x)
    xoutliers <- tryCatch({
      Outlier(df[[x]])
    },
    error = function(cond) {
      paste0("No outliers found in `", x, "`")
    })

    xhistogram <- autoHistogramPlot_(df, target = x)

    reportobjs <- list(name = x, type = xtype, length = xlength,
                       summary = xsummary, correlations = xcorrelations,
                       outliers = xoutliers, histogram = xhistogram)
  }

}



reportgenerator <- function(df, x) {

  variableObject <- variableSummary(df, x)


}
