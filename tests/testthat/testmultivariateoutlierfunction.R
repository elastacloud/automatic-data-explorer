
context("Testing the multivariate outlier function")

test_that("The OptimalEpsilon function works with a dataset with outliers and NAs",{

  # test data
  mtcars <- mtcars
  mtcars_outliers <- data.frame(mpg = c(50.02,52.56,60.050), cyl= c(12.122,15.555,17.888), disp = c(500.5,505.9,510.4),hp = c(500.6,600.7,550.8), drat = c(8.777,10.555,12.666), wt = c(10.333,11.324,12.676), qsec = c(30.89,33.45,35.67), vs= c(1.2345,1.4555,1.6666), am = c(1.2345,1.4555,1.6666), gear = c(10.233,11.233,12.233), carb = c(12.343,15.677,17.878))
  mtcars_NAs <- data.frame(mpg = c(50.02,NA,60.050), cyl= c(NA,15.555,17.888), disp = c(500.5,505.9,NA),hp = c(NA,600.7,550.8), drat = c(8.777,NA,12.666), wt = c(NA,11.324,12.676), qsec = c(30.89,33.45,NA), vs= c(1.2345,NA,1.6666), am = c(NA,1.4555,1.6666), gear = c(10.233,NA,12.233), carb = c(NA,15.677,17.878))
  mtcars2 <- rbind(mtcars, mtcars_outliers, mtcars_NAs)

  epsilon <- OptimalEpsilon(mtcars2)

  expect_that(class(epsilon), equals("numeric"))
  expect_that(epsilon, equals(2))
})


test_that("The OptimalMinPoints function works with a dataset with outliers and NAs",{

  #test data
  mtcars <- mtcars
  mtcars_outliers <- data.frame(mpg = c(50.02,52.56,60.050), cyl= c(12.122,15.555,17.888), disp = c(500.5,505.9,510.4),hp = c(500.6,600.7,550.8), drat = c(8.777,10.555,12.666), wt = c(10.333,11.324,12.676), qsec = c(30.89,33.45,35.67), vs= c(1.2345,1.4555,1.6666), am = c(1.2345,1.4555,1.6666), gear = c(10.233,11.233,12.233), carb = c(12.343,15.677,17.878))
  mtcars_NAs <- data.frame(mpg = c(50.02,NA,60.050), cyl= c(NA,15.555,17.888), disp = c(500.5,505.9,NA),hp = c(NA,600.7,550.8), drat = c(8.777,NA,12.666), wt = c(NA,11.324,12.676), qsec = c(30.89,33.45,NA), vs= c(1.2345,NA,1.6666), am = c(NA,1.4555,1.6666), gear = c(10.233,NA,12.233), carb = c(NA,15.677,17.878))
  mtcars2 <- rbind(mtcars, mtcars_outliers, mtcars_NAs)

  minPts <- OptimalMinPoints(mtcars2)

  expect_that(class(minPts), equals("list"))
  expect_that(minPts$minPts, equals(6))
  expect_that(minPts$epsilon, equals(2))
})

test_that("The MultivaraiteOutlier function works with a dataset with outliers and NAs",{

  #test data
  mtcars <- mtcars
  mtcars_outliers <- data.frame(mpg = c(50.02,52.56,60.050), cyl= c(12.122,15.555,17.888), disp = c(500.5,505.9,510.4),hp = c(500.6,600.7,550.8), drat = c(8.777,10.555,12.666), wt = c(10.333,11.324,12.676), qsec = c(30.89,33.45,35.67), vs= c(1.2345,1.4555,1.6666), am = c(1.2345,1.4555,1.6666), gear = c(10.233,11.233,12.233), carb = c(12.343,15.677,17.878))
  mtcars_NAs <- data.frame(mpg = c(50.02,NA,60.050), cyl= c(NA,15.555,17.888), disp = c(500.5,505.9,NA),hp = c(NA,600.7,550.8), drat = c(8.777,NA,12.666), wt = c(NA,11.324,12.676), qsec = c(30.89,33.45,NA), vs= c(1.2345,NA,1.6666), am = c(NA,1.4555,1.6666), gear = c(10.233,NA,12.233), carb = c(NA,15.677,17.878))
  mtcars2 <- rbind(mtcars, mtcars_outliers, mtcars_NAs)

  outliers <- MultivariateOutlier(mtcars)

  expect_that(class(outliers), equals("list"))
  expect_that(ncol(outliers$dfOutliers), equals(12))
  expect_that(nrow(outliers$dfOutliers), equals(32))
  expect_that(outliers$dfOutliers[[1,1]], equals(21))
  expect_that(outliers$dfOutliers[[4,4]], equals(110))
})
