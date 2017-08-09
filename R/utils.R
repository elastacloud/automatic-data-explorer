

# Generates the code chunks for .Rmd file
writeChunk <- function(object, filename,
                       chunktitle = NULL,
                       quiet = FALSE) {

  topline <- ifelse(quiet,
                    "```{r echo = F, message = F} ",
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
