
context("Testing visualisation functions")

test_that("Histogram visualisation function",{

  library(ggplot2)

  set.seed(12)

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1))

  plot <- autoHistogramPlot(testdata, "A")
  plot_2 <- autoHistogramPlot(testdata, "C", interactiveplot = T)

  expect_true(all(class(plot) == c("gg", "ggplot")))
  expect_true(all(class(plot_2) == c("plotly", "htmlwidget")))


})


test_that("Correlation plot function", {

  m <- multivariateCorrelation(mtcars, output = "matrix")

  plot <- autoCorrelationPlot(m)
  plot_2 <- autoCorrelationPlot(m, interactiveplot = T)

  expect_true(class(plot) == "matrix")
  expect_true(all(class(plot_2) == c("plotly", "htmlwidget")))

  m_A <- m
  m_A[2, 1] <- 0.654

  m_B <- m
  m_B[2, 1] <- -1.12

  expect_error(autoCorrelationPlot(m_A))
  expect_error(autoCorrelationPlot(m_B, interactiveplot = T))

})
