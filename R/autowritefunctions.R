

insertChunk <- function(filename, object,
                       title = NULL,
                       con = NULL) {

  do <- deparse(substitute(object))

  if(is.null(title)) {
    write(c("```{r}", do, "```"), file = filename, append = TRUE)
  } else {
    write(c(title,"```{r}", do, "```"), file = filename, append = TRUE)
  }

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
