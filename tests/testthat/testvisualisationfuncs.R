
context("Testing visualisation functions")

test_that("Histogram visualisation function",{

  library(ggplot2)

  set.seed(12)

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1))

  plot <- gghistogram(testdata, "A")
  plot_2 <- gghistogram(testdata, "C", interactiveplot = T)

  expect_true(all(class(plot) == c("gg", "ggplot")))
  expect_true(all(class(plot_2) == c("plotly", "htmlwidget")))

})
