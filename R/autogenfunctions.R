
#' Auto create R Markdown documents
#' @description Automatically generate data exploration reports using
#' R Markdown templates.
#' @param df Dataframe containing the variables of interest
#' @param target For univariate reporting - the target variable
#' @param template The name of the template document to use. Currently only \code{"univariate"}
#' is available
#' @param reporttitle Character giving the report title in the YAML header
#' @param reportauthor Character giving the report author's name in the YAML
#' header
#' @param documentname Character giving the filename of the generated report
#' @param documentwd Character giving the directory to which the report should
#' be written
#' @param writedoc Default \code{writedoc = TRUE} will auto generate the final document.
#' Changing to \code{writedoc = FALSE} will prevent the input .Rmd from being rendered to the
#' specified input (currently only .html) to allow review
#' @return If \code{writedoc = TRUE} writes .Rmd file and auto generated document to \code{documentwd}.
#' If \code{writedoc = FALSE} writes .Rmd file only.
createDocument <- function(df, target, template, reporttitle, reportauthor,
                         documentname, documentwd,
                         writedoc = TRUE) {

  ## TODO {Andrew} Allow use of NSE for df and target. Add more templates. Add options for different
  ## outputs (i.e. pdf, word etc.). Option to decide plots interactive. Error handling. File copying etc.

  if(!template %in% c("univariate")) {
    stop(template," not recognised template type", call. = FALSE)
  }

  if(template == "univariate") {

    intemplate <- readLines("./templates/univariateTemplate.Rmd")

  }

  intemplate <- edittemplate(template = intemplate, df = df, target = target,
                             reporttitle = reporttitle,
                             reportauthor = reportauthor)

  if(!writedoc) {
    return(intemplate)
  } else {
    writedocument(editedtemplate = intemplate, name = documentname,
                  wd = documentwd)
  }

}


writedocument <- function(editedtemplate, name, wd) {

  outlocation <- paste(wd, name, sep = "")
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
