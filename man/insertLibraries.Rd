% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autowritefunctions.R
\name{insertLibraries}
\alias{insertLibraries}
\title{Insert attach package code chunk}
\usage{
insertLibraries(filename, libraries = c("AutoExploreR", "ggplot2", "plotly"))
}
\arguments{
\item{filename}{The name of the file, including the .Rmd extension, into
which the code chunk should be inserted}

\item{libraries}{Character vector giving the names of the packages to attach}
}
\value{
Returns the filename to allow chaining with other insert functions
}
\description{
Insert attach package code chunk
}
