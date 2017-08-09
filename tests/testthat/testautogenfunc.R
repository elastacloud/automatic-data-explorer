
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


test_that("Create file function works correctly", {

  createFile("Test File")

  expect_true(file.exists("Test File.Rmd"))

  expect_error(createFile("Test File Two", filedir = "c:/No/Directory/Here"))

  file.remove("Test File.Rmd")
})


test_that("Test insert functions work correctly", {

  createFile("Testing File Two") %>% insertYAML(title = "Testing", author = "Andrew") %>%
    insertLibraries() %>% insertChunk(expression(summary(mtcars), plot(mtcars$hp)))

  expect_error(insertChunk("NotAFile.Rmd", summary(mtcars)))

  expect_true(file.exists("Testing File Two.Rmd"))


  fileread <- readLines("Testing File Two.Rmd")

  expect_that(length(fileread), equals(17))

  expect_true(stringr::str_detect(fileread[4], as.character(Sys.Date())))


  ## Test insertQuietChunk

  createFile("Testing File Three") %>% insertYAML(title = "Testing", author = "Andrew") %>%
    insertLibraries() %>% insertQuietChunk(expression(summary(mtcars), plot(mtcars$hp)))

  filereadtwo <- readLines("Testing File Three.Rmd")

  expect_false(identical(fileread, filereadtwo))

  expect_equal(sum(stringr::str_detect(filereadtwo, "echo")), 3)
  expect_equal(sum(stringr::str_detect(filereadtwo, "message")), 3)

  file.remove("Testing File Two.Rmd")
  file.remove("Testing File Three.Rmd")
})



