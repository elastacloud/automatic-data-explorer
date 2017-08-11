


#' Insert a YAML header into the R Markdown file
#' @param filename The name of the file, including the .Rmd extension, into
#' which the YAML header should be inserted
#' @param title Character giving the title of the document
#' @param author Character giving the author's name
#' @param date If \code{NULL} inserts \code{Sys.Date()} into the date field.
#' @param output The document type that will be generated, e.g. \code{"html_document",
#' "pdf"}
#' @export
#' @return Returns the filename to allow chaining with other insert functions
#' @examples
#' \dontrun{
#'
#' createFile("Test File") %>%
#'           insertYAML(title = "Test Report",
#'                      author = "J. Bloggs",)
#' }
insertYAML <- function(filename,
                       title = NULL, author = NULL, date = NULL,
                       output = "html_document") {

  if(is.null(date)) {
    date <- Sys.Date()  # Use system date if not given
  }

  title <- paste0("title: ", title)
  author <- paste0("author: ", author)
  output <- paste0("output: ", output)
  date <- paste0("date: ", "'", date, "'")

  header <- c("---", title, author, date, output, "---")

  write(header, file = filename, append = TRUE)

  filename
}


#' Insert attach package code chunk
#' @param filename The name of the file, including the .Rmd extension, into
#' which the code chunk should be inserted
#' @param libraries Character vector giving the names of the packages to attach
#' @export
#' @return Returns the filename to allow chaining with other insert functions
insertLibraries <- function(filename,
                            libraries = c("AutoExploreR", "ggplot2",
                                          "plotly")) {

  # Error if .Rmd file does not exist
  if(!file.exists(filename)) {
    stop("R Markdown file `", filename, "` not found", call. = FALSE)
  }

  # Paste library names into library function call
  libraries <- purrr::map_chr(libraries, ~ paste0("library(", ., ")"))

  write(c("```{r echo = F, message = F}", libraries, "```"), file = filename, append = TRUE)

  filename
}


#' Generate a named R Markdown file in a given directory
#' @param filename Character giving the desired name of the R Markdown file, does
#' not require the \code{.Rmd} extension
#' @param filedir Default \code{"current"} will write the file to the current working
#' directory, given by \code{getwd()}. Provide a character to give an alternative
#' @export
#' @return Returns the name of the file, including the \code{.Rmd} extension so that
#' insert functions can be chained after
createFile <- function(filename,
                       filedir = "current") {

  rmdname <- paste0(filename, ".Rmd")

  # Use current working directory if specific one not given
  if(filedir == "current") {
    file.create(file.path(getwd(), rmdname))
  } else {
    if(!dir.exists(filedir)) {
      stop("Directory `", filedir, "` does not exist", call. = FALSE)
    } else {
      file.create(file.path(filedir, rmdname))
    }

  }

  rmdname
}







