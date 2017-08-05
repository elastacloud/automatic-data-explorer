# Automatic Data Explorer  [![Build Status](https://travis-ci.org/elastacloud/automatic-data-explorer.svg?branch=master)](https://travis-ci.org/elastacloud/automatic-data-explorer)  [![codecov](https://codecov.io/gh/elastacloud/automatic-data-explorer/branch/master/graph/badge.svg)](https://codecov.io/gh/elastacloud/automatic-data-explorer)

An R package to explore and quality check data. Contains a variety of useful functions which enable automatic checking of data quality, factors and numeric data as well as correlations.

- targetCorrletions 
- ggdensity
- gghistogram
- SummaryStatsCat
- SummaryStatsNum

## Using targetCorrelations

To get started use a data frame and detail the column that you want to get target correlations for:

    install.packages("purrr")
    library(purrr)

    data <- data.frame(A = rnorm(50,0,1),
                       B = runif(50,10,20),
                       C = seq(1,50,1),
                       D = rep(LETTERS[1:5], 10))

    targetCorrelations(data, "B")

This should give a similar report to:

             C          A 
    0.40549008 0.01356416 



