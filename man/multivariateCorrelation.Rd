% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlationfunctions.R
\name{multivariateCorrelation}
\alias{multivariateCorrelation}
\title{Calculate correlations between numeric variables}
\usage{
multivariateCorrelation(df, dropnotnumeric = TRUE, output = "ranked",
  N = NULL, use = "everything", method = c("pearson", "kendall",
  "spearman"))
}
\arguments{
\item{df}{Dataframe containing all variables.}

\item{dropnotnumeric}{Logical, defaults to \code{TRUE}. Automatically drop any non-numeric variables
from the dataframe.}

\item{output}{Determines which output type to return. One of \code{ranked} for ranked correlation
pairs or \code{matrix} for the correlation matrix.}

\item{N}{Only used when \code{output = "ranked"}. Default \code{NULL} will return all variable pair
correlations. Change to e.g. \code{N = 5} to return only the 5 highest absolute correlations}

\item{use}{Same as \code{use} in the \code{cor} function.}

\item{method}{Same as \code{method} in the \code{cor} function.}
}
\value{
Returns the correlation matrix if \code{output = "matrix"} or dataframe of ordered
variable pair correlations if \code{output = "ranked"}.
}
\description{
Can return one of two objects. 1. Dataframe of ranked
absolute correlation pairs, or 2. Correlation matrix of all variables.
}
