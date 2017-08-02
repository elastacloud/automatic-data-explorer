
context("Testing summary functions")

test_that("Test the SummaryStatsNum function on a dataset with NAs",{

  b <- c(1, 2,  3,  4,  5,  1,  2,  3,  4,  5, 50, 55, NA, NA)

  A <- SummaryStatsNum(b)

  test_that(A[1], equals(2))
  test_that(A[2], equals(11.25))
  test_that(A[3], equals(19.3443673))
  test_that(A[4], equals(3.5))
  test_that(A[5], equals(1.0))
  test_that(A[6], equals(55.00))
  test_that(A[7], equals(1.00))
  test_that(A[8], equals(55.00))
  test_that(A[9], equals(1.5635253))
  test_that(A[10], equals(0.5545518))
  test_that(length(A), equals(10))

})

test_that("Test the SummaryStatsCat function on a dataset with NAs",{

  cat <- as.factor(c("maths", "english", "science", "maths", "english", "science", "maths", "english", "science", "art", "PE", NA, NA))

  B <- SummaryStatsCat(cat)

  test_that(B[1,1], equals(1))
  test_that(B[2,2], equals(23.076923))
  test_that(B[6,2], equals(15.384615))
  test_that(dim(B), equals(6,2))
})
