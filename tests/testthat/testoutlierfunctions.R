
context("Testing outlier functions")

test_that("Test the Outlier function on a dataset with outliers and NAs",{

  b <- c(1, 2,  3,  4,  5,  1,  2,  3,  4,  5, 50, 55, NA, NA)

  outliers <- Outliers(b)

  expect_that(class(outliers), equals("list"))

  expect_that(length(outliers$MildOutliers), equals(2))
  expect_that(outliers$MildOutliers[1,1], equals(11))
  expect_that(outliers$MildOutliers[2,2], equals(55))

  expect_that(length(outliers$ExtremeOutliers), equals(2))
  expect_that(outliers$ExtremeOutliers[1,1], equals(11))
  expect_that(outliers$ExtremeOutliers[2,2], equals(55))

  expect_that(length(outliers$MildThresholds), equals(2))
  #expect_that(outliers$MildThresholds[1,1], equals(-2.5))
  expect_that(outliers$MildThresholds[[1]], equals(-2.5))
  expect_that(outliers$MildThresholds[[2]], equals(9.5))

  expect_that(length(outliers$ExtremeThresholds), equals(2))
  expect_that(outliers$ExtremeThresholds[[1]], equals(-7))
  expect_that(outliers$ExtremeThresholds[[2]], equals(14))
})


test_that("Test the OutlierMean function works on a dataset with outliers and NAs",{

  b <- c(1, 2,  3,  4,  5,  1,  2,  3,  4,  5, 50, 55, NA, NA)

  outlierMean <- OutlierMean(b)

  expect_that(class(outlierMean), equals("data.frame"))
  expect_that(outlierMean[1,1], equals(11.25))

})

test_that("Test the OutlierPercentage function works on a dataset with outliers and NAs",{

  b <- c(1, 2,  3,  4,  5,  1,  2,  3,  4,  5, 50, 55, NA, NA)

  outlierPercentage <- OutlierPercentage(b)

  expect_that(class(outlierPercentage), equals("data.frame"))
  expect_equal(outlierPercentage[1,2],14.28571, tolerance = 1e-3)

})

test_that("Test the Outlier main function works on a dataset with outliers and NAs",{

  b <- c(1, 2,  3,  4,  5,  1,  2,  3,  4,  5, 50, 55, NA, NA)

  outlier <- Outlier(b)

  expect_that(class(outlier), equals("list"))

})
###################################################################
test_that("Test the Outlier function on a dataset with no outliers",{

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1),
                         D = rep(LETTERS[1:5], 10))

  C <- testdata$B

  expect_error(Outliers(C))
})

test_that("Test the OutlierMean function on a dataset with no outliers",{

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1),
                         D = rep(LETTERS[1:5], 10))

  D <- testdata$B

  expect_error(OutlierMean(D))
})

test_that("Test the OutlierPercentage function on a dataset with no outliers",{

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1),
                         D = rep(LETTERS[1:5], 10))

  E <- testdata$B

  expect_error(OutlierPercentage(E))
})

test_that("Test the Outlier main function works with a dataset with no outliers",{

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1),
                         D = rep(LETTERS[1:5], 10))

  G <- testdata$B


  expect_error(Outlier(G))
})

