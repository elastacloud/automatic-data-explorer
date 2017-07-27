

readtemplate <- function(template, df, target, reporttitle, reportauthor) {

  if(template == "univariate") {

    intemplate <- readLines("./templates/univariateTemplate.Rmd")

  }

  intemplate <- edittemplate(template = intemplate, df = df, target = target,
                             reporttitle = reporttitle,
                             reportauthor = reportauthor)

  intemplate

}


writetemplate <- function(editedtemplate, name, wd) {

  outlocation <- paste(wd, name, sep = "")
  writeLines(text = editedtemplate, con = outlocation)

}


edittemplate <- function(template, df, target, reporttitle, reportauthor) {

  template[grep("title: Test", template)] <- paste0("title: ", reporttitle)
  template[grep("author: Andrew", template)] <- paste0("author: ", reportauthor)

  template <- gsub("(?<=\')variableName(?=\')", target, template, perl = T)

  template <- gsub("dataName", df, template)

  template

}
