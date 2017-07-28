
#' Insert a code chunk into an R Markdown file
#' @description This function can be used to insert any line of code
#' into an R Markdown file. The chunk will be inserted into the file called
#' \code{filename}, which must exist in the current working directory. Chaining
#' with \code{\%>\%} is possible, by returning \code{filename} from the function.
#' @param filename Character giving the name of the R Markdown file that the
#' chunk should be inserted into, minus the \code{.Rmd} extension
#' @param object Can be any unquoted line of R code, i.e. a call to a function
#' @param title Character giving an optional markdown header that will can
#' included before the code chunk e.g. of the form \code{# Header, ## Header,
#' ### Header}
#' @return Returns \code{filename} so that functions can be chained to write
#' to the same file consecutively
insertChunk <- function(filename, object,
                       title = NULL) {

  do <- deparse(substitute(object))

  if(is.null(title)) {
    write(c("```{r}", do, "```"), file = filename, append = TRUE)
  } else {
    write(c(title,"```{r}", do, "```"), file = filename, append = TRUE)
  }

  filename

}


insertYAML <- function(filename,
                       title = NULL, author = NULL, date = NULL,
                       output = "html_document") {

  if(is.null(date)) {
    date <- Sys.Date()
  }

  title <- paste0("title: ", title)
  author <- paste0("author: ", author)
  output <- paste0("output: ", output)
  date <- paste0("date: ", "'", date, "'")

  header <- c("---", title, author, date, output, "---")

  write(header, file = filename, append = TRUE)

  filename
}

insertLibraries <- function(filename,
                            libraries = c("AutoExploreR", "ggplot2",
                                          "plotly")) {

  libraries <- purrr::map_chr(libraries, ~ paste0("library(", ., ")"))

  write(c("```{r}", libraries, "```"), file = filename, append = TRUE)

  filename
}




createFile <- function(filename,
                       filedir = "current") {

  rmdname <- paste0(filename, ".Rmd")

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

openFile <- function(filename,
                     filedir = "current") {

  rmdname <- paste0(filename, ".Rmd")

  if(filedir == "current") {
    tmp <- file(file.path(getwd(), rmdname))
  } else {
    if (!dir.exists(filedir)) {
      stop("Directory `", filedir, "` does not exist", call. = FALSE)
    } else {
      tmp <- file(file.path(filedir, rmdname))
    }

  }

  open(tmp, "w")

  if(isOpen(tmp)) {
    message("Connection to `", rmdname, "` successfully opened")
  } else {
    stop("Connection `", rmdname, "` could not be made", call. = FALSE)
  }

 # return(tmp)

}
