
context("Testing visualisation functions")

test_that("Histogram visualisation function",{

  library(ggplot2)

  set.seed(12)

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1),
                         D = rep(LETTERS[1:5], 10))

  plot <- autoHistogramPlot(testdata, "A", fill = "blue", colour = "black")
  plot_2 <- autoHistogramPlot(testdata, "C", interactiveplot = T)
  plot_3 <- autoHistogramPlot(testdata, "D", xlabel = "Some Letters")

  expect_true(all(class(plot) == c("gg", "ggplot")))
  expect_true(all(class(plot_2) == c("plotly", "htmlwidget")))

  expect_true(any(class(plot$layers[[1]]$stat) == "StatBin"))

  expect_equal(plot_3$labels[[1]], "Some Letters")
  expect_true(any(class(plot_3$layers[[1]]$stat) == "StatCount"))
  expect_true(all(purrr::map_chr(plot$layers[[1]]$aes_params, ~ .) == c("blue", "black")))

})

test_that("Density plot function", {

  library(ggplot2)

  set.seed(12)

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1),
                         D = rep(LETTERS[1:5], 10))

  plot <- autoDensityPlot(testdata, A)
  plot_2 <- autoDensityPlot(testdata, B, interactiveplot = T)
  plot_3 <- autoDensityPlot(testdata, C, colour = "black", xlabel = "Variable C")

  expect_error(autoDensityPlot(testdata, D))

  expect_true(all(class(plot) == c("gg", "ggplot")))
  expect_true(all(class(plot_2) == c("plotly", "htmlwidget")))

  expect_true(any(class(plot$layers[[1]]$stat) == "StatDensity"))
  expect_equal(plot_3$labels[[1]], "Variable C")
  expect_true(plot_3$layers[[1]]$aes_params == "black")

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
