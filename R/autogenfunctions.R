
## TODO: add an overwrite option so that doesn't just constantly append to
# the end of the the rmd file

#' Automated generation of R Markdown documents from R Scripts
#' @description Quickly and easily generate R Markdown documents from an R script.
#' Combine any number of R scripts into a single R Markdown document.
#' @param filename Either a single, or vector of, R script filenames in the order which you wish them
#' to be written to R Markdown
#' @param rmdfile The name of the rmdfile to write to, with the \code{.Rmd} extension
#' @param quiet The default is \code{quiet = TRUE} which creates 'quiet' chunks. These code chunk will display
#' the results of the code only
#' @param render Automatically render the resulting R Markdown document
#' @param overwrite If \code{overwrite = TRUE} the provided \code{.Rmd} file will be completely overwritten
#' @param divider A unique character used to identify where the user wishes to split their script into separate
#' code chunks and comment sections
#' @export
autoMarkdown <- function(filename, rmdfile = NULL,
                         quiet = TRUE, render = FALSE,
                         overwrite = FALSE, divider = "#'#") {

  filesexist <- file.exists(filename)

  if (!any(filesexist)) {
    stop("Files not found in current directory: ", getwd(), call. = FALSE)
  }

  if (is.null(rmdfile)) {
      stop("Please provide a name for the .Rmd file", call. = FALSE)
  }

  if (overwrite) {
    write("", rmdfile, append = FALSE)
  }

  for (i in seq_along(filename)) {
    writermd(filename[i], rmdfile, quiet, divider)
  }

  rmdfile
}














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




