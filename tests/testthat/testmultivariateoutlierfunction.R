
context("Testing multivariate outleir function")

test_that("The multivariate outlier function works with a dataset with NAs",{

  # inject outliers and NAs into mtcars
  mtcars <- mtcars
  mtcars_outliers <- data.frame(mpg = c(50.02,52.56,60.050), cyl= c(12.122,15.555,17.888), disp = c(500.5,505.9,510.4),hp = c(500.6,600.7,550.8), drat = c(8.777,10.555,12.666), wt = c(10.333,11.324,12.676), qsec = c(30.89,33.45,35.67), vs= c(1.2345,1.4555,1.6666), am = c(1.2345,1.4555,1.6666), gear = c(10.233,11.233,12.233), carb = c(12.343,15.677,17.878))
  mtcars_NAs <- data.frame(mpg = c(50.02,NA,60.050), cyl= c(NA,15.555,17.888), disp = c(500.5,505.9,NA),hp = c(NA,600.7,550.8), drat = c(8.777,NA,12.666), wt = c(NA,11.324,12.676), qsec = c(30.89,33.45,NA), vs= c(1.2345,NA,1.6666), am = c(NA,1.4555,1.6666), gear = c(10.233,NA,12.233), carb = c(NA,15.677,17.878))
  mtcars2 <- rbind(mtcars, mtcars_outliers, mtcars_NAs)

  Outliers <- MultivariateOutlier(mtcars2)

  expect_that(class(Outliers), equals("list"))
  expect_that(ncol(Outliers$DFOutliers), equals(12))
  expect_that(nrow(Outliers$DFOutliers), equals(38))
  expect_that(Outliers$DFOutliers[[5,1]], equals(18.7))
  expect_that(Outliers$DFOutliers[[4,3]], equals(258))
})

