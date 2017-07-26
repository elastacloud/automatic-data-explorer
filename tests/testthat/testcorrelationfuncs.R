
context("Test the target correlation function")

test_that("Test the target correlation function behaves correctly",{

  set.seed(12)

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1),
                         D = rep(LETTERS[1:5], 10))


  expect_error(targetCorrelations(testdata, "D"))

  expect_warning(targetCorrelations(testdata, "A", N = 10))

  expect_that(length(targetCorrelations(testdata, "B", N = 1)), equals(1))

  corrs <- targetCorrelations(testdata, "A")

  expect_that(length(corrs), equals(2))

  expect_true(abs(corrs[1]) >= abs(corrs[2]))

})



test_that("Test the multivariate correlation function behaves correctly",{

  set.seed(12)

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1),
                         D = rep(LETTERS[1:5], 10))

  # errors

  expect_error(multivariateCorrelation(testdata, dropnotnumeric = FALSE))
  expect_error(multivariateCorrelation(testdata, output = "Rabked"))


  # ranked return type

  ranked <- multivariateCorrelation(testdata, output = "ranked")

  expect_true(is.data.frame(ranked))
  expect_that(nrow(ranked), equals(sum(sapply(testdata, is.numeric))))


  # matrix return type

  mat <- multivariateCorrelation(testdata, output = "matrix")

  expect_true(is.matrix(mat))
  expect_that(dim(ranked), equals(c(sum(sapply(testdata, is.numeric)),
                                    sum(sapply(testdata, is.numeric)))))

})
