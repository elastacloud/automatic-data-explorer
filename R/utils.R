

# Generates the code chunks for .Rmd file
writeChunk <- function(object, filename,
                       chunktitle = NULL,
                       quiet = FALSE) {

  topline <- ifelse(quiet,
                    "```{r echo = F, message = F, warning = F} ",
                    "```{r}")

  if(is.null(chunktitle)) {
    write(c(topline, object, "```"), file = filename, append = TRUE)
  } else {
    write(c(chunktitle,topline, object, "```"), file = filename, append = TRUE)
  }

}



writedocument <- function(editedtemplate, name, wd) {

  outlocation <- file.path(wd, name)    #paste(wd, name, sep = "")
  writeLines(text = editedtemplate, con = outlocation)
  rmarkdown::render(outlocation)

}


edittemplate <- function(template, df, target, reporttitle, reportauthor) {

  template[grep("title: Test", template)] <- paste0("title: ", reporttitle)
  template[grep("author: Andrew", template)] <- paste0("author: ", reportauthor)

  template <- gsub("(?<=\')variableName(?=\')", target, template, perl = T)

  template <- gsub("dataName", df, template)

  template

}


rmdtype <- function(line, filename, quiet = TRUE) {

  ## TODO: This needs to be more robust

  if(any(stringr::str_detect(line, "#'"))) {
    write(stringr::str_replace(line, "#' *", ""), file = filename, append = T)
  } else {
    if (quiet) {
      insertQuietChunk(filename = filename, line)
    } else {
      insertChunk(filename = filename, line)
    }

  }

  filename
}


writermd <- function(filename, rmdfile, quiet, divider) {

  readfile <- readLines(filename, warn = FALSE)

  idxs <- which(readfile == divider)
  lenidx <- length(idxs)

  outp <- vector(mode = "list", length = lenidx - 1)

  for (i in 1:(lenidx - 1)) {
    outp[[i]] <- readfile[(idxs[i] + 1) : (idxs[i + 1] - 1)]
  }

  purrr::map_chr(outp, ~ rmdtype(., rmdfile, quiet))

}
