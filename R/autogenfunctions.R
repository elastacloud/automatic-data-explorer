
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
#' @export
createDocument <- function(df, target, template, reporttitle, reportauthor,
                         documentname, documentwd,
                         writedoc = TRUE) {

  ## TODO {Andrew} Add more templates. Add options for different
  ## outputs (i.e. pdf, word etc.). Option to decide plots interactive. Error handling. File copying etc.

  # Non standard evaluation of input df and target names allowing interactive use

  ### ## NEED TO CHECK ALL THIS FOR POTENTIAL ERRORS

  target <- substitute(target)
  dfname <- deparse(substitute(df))  # Create character from name of input dataframe

  if(is.symbol(target)) {
    target <- deparse(target)
  }

  ###

  if(!template %in% c("univariate")) {
    stop(template," not recognised template type", call. = FALSE)
  }

  if(!dir.exists(documentwd)) {
    stop("Directory '", documentwd , "' not found, please choose another", call. = FALSE)
  }

  templatename <- paste0(template, "Template.Rmd")

  intemplate <- readLines(system.file("extdata", templatename,
                                        package = "AutoExploreR"))

  intemplate <- edittemplate(template = intemplate, df = dfname, target = target,
                             reporttitle = reporttitle,
                             reportauthor = reportauthor)

  if(!writedoc) {
    return(intemplate)
  } else {
    writedocument(editedtemplate = intemplate, name = documentname,
                  wd = documentwd)
  }

}




