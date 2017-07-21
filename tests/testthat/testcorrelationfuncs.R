
context("Test the target correlation function")

test_that("Test the target correlation function returns correct object",{

  set.seed(12)

  testdata <- data.frame(A = rnorm(50,0,1),
                         B = runif(50,10,20),
                         C = seq(1,50,1))

  corrs <- targetCorrelations(testdata, "A")



  expect_that(length(corrs), equals(2))

  expect_true(abs(corrs[1]) >= abs(corrs[2]))

})
