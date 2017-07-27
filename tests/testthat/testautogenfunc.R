
context("Test the document auto generation function")

test_that("Test error handling of auto generation function", {

  # Directory does not exist
  expect_error(
    createDocument(template = "univariate", df = "testdata", target = "B",
                   reporttitle = "Test Report", reportauthor = "Andrew Booth",
                   documentname = "TestDoc.Rmd",
                   documentwd = "C:\\Doesnotexist\\", writedoc = T)
  )

  # Wrong template name
  expect_error(
    createDocument(template = "unrate", df = "testdata", target = "B",
                   reporttitle = "Test Report", reportauthor = "Andrew Booth",
                   documentname = "TestDoc.Rmd",
                   documentwd = "C:\\Doesnotexist\\", writedoc = T)
  )

})
