


#' Insert a code chunk into an R Markdown file
#' @description This function can be used to insert any line or lines of code
#' into an R Markdown file. The chunk will be inserted into the file called
#' \code{filename.Rmd}, which must exist in the current working directory. Chaining
#' with \code{\%>\%} is possible, by returning \code{filename} from the function.
#' @param filename Character giving the name of the R Markdown file that the
#' chunk should be inserted into, minus the \code{.Rmd} extension
#' @param object Can be any quoted or unquoted line of R code, i.e. a call to a function to insert a
#' single line. To insert more than one line, pass as a vector of type \code{"expression"}.
#' @param chunktitle Single or vector of character/s giving an optional markdown header that will be
#' included before the code chunk e.g. of the form \code{"# Header", "## Header",
#' "### Header"}. Pass as a vector of titles of equal length to object expressions
#' if multiple headers required.
#' @export
#' @return Returns \code{filename} so that functions can be chained to write
#' to the same file consecutively
#' @examples
#'
#' \dontrun{
#'
#' ## Insert single chunk, chained from createFile() function
#'
#' createFile("Mtcars") %>% insertYAML(title = "Test", author = "J. Bloggs") %>%
#'                          insertLibraries() %>%
#'                          insertChunk(summary(mtcars), title = "#Summary")
#'
#'                          insertChunk("summary(mtcars)", title = "#Summary") would also work
#'
#' ## Insert multiple chunks to existing file, with headers
#'
#' insertChunk("Mtcars.Rmd",
#'                  expression(
#'                       plot(mtcars$cyl, mtcars$hp),
#'                       multivariateCorrelation(mtcars)),
#'                             title= c("##Scatter Plot",
#'                                      "##Correlations"))
#' }
insertChunk <- function(filename, object,
                        chunktitle = NULL) {

  # Error if .Rmd file does not exist
  if(!file.exists(filename)) {
    stop("R Markdown file `", filename, "` not found", call. = FALSE)
  }

  if(any(!stringr::str_detect(chunktitle, "#"))) {
    warning("Titles without # will not render correctly")
  }

  if (is.character(object)) {
    writeChunk(object, filename = filename, chunktitle = chunktitle)
    return(filename)
  }

  if(is.expression(object)) {
    deparsed <- purrr::map_chr(object, ~ deparse(.))  # Deparse the provided expressions

    for(i in seq_along(deparsed)) {
      writeChunk(deparsed[[i]], filename = filename, chunktitle = chunktitle[[i]])
    }
  } else {
    do <- deparse(substitute(object))
    writeChunk(do, filename = filename, chunktitle = chunktitle)
  }

  filename

}




#' Insert a 'quiet' code chunk into an R Markdown file
#' @description This function can be used to insert any line or lines of code
#' into an R Markdown file 'quietly'. The chunk will be inserted into the file called
#' \code{filename.Rmd}, which must exist in the current working directory. Chaining
#' with \code{\%>\%} is possible, by returning \code{filename} from the function.
#' \code{insertQuietChunk} will insert a code chunk that will only show the results
#' of the code in the rendered document.
#' @param filename Character giving the name of the R Markdown file that the
#' chunk should be inserted into, minus the \code{.Rmd} extension
#' @param object Can be any quoted or unquoted line of R code, i.e. a call to a function to insert a
#' single line. To insert more than one line, pass as a vector of type \code{"expression"}.
#' @param chunktitle Single or vector of character/s giving an optional markdown header that will be
#' included before the code chunk e.g. of the form \code{"# Header", "## Header",
#' "### Header"}. Pass as a vector of titles of equal length to object expressions
#' if multiple headers required.
#' @export
#' @return Returns \code{filename} so that functions can be chained to write
#' to the same file consecutively
#' @examples
#'
#' \dontrun{
#'
#' ## Insert single chunk, chained from createFile() function
#'
#' createFile("Mtcars") %>% insertYAML(title = "Test", author = "J. Bloggs") %>%
#'                          insertLibraries() %>%
#'                          insertQuietChunk(summary(mtcars), title = "#Summary")
#'
#'                          insertChunk("summary(mtcars)", title = "#Summary") would also work
#'
#' ## Insert multiple chunks to existing file, with headers
#'
#' insertQuietChunk("Mtcars.Rmd",
#'                  expression(
#'                       plot(mtcars$cyl, mtcars$hp),
#'                       multivariateCorrelation(mtcars)),
#'                             title= c("##Scatter Plot",
#'                                      "##Correlations"))
#' }
insertQuietChunk <- function(filename, object,
                        chunktitle = NULL) {

  # Error if .Rmd file does not exist
  if(!file.exists(filename)) {
    stop("R Markdown file `", filename, "` not found", call. = FALSE)
  }

  if(any(!stringr::str_detect(chunktitle, "#"))) {
    warning("Titles without # will not render correctly")
  }

  if (is.character(object)) {
    writeChunk(object, filename = filename, chunktitle = chunktitle, quiet = TRUE)
    return(filename)
  }

  if(is.expression(object)) {
    deparsed <- purrr::map_chr(object, ~ deparse(.))  # Deparse the provided expressions

    for(i in seq_along(deparsed)) {
      writeChunk(deparsed[[i]], filename = filename, chunktitle = chunktitle[[i]], quiet = TRUE)
    }
  } else {
    do <- deparse(substitute(object))
    writeChunk(do, filename = filename, chunktitle = chunktitle, quiet = TRUE)
  }

  filename

}
