% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getUniqueColumns.R
\name{getUniqueColumns}
\alias{getUniqueColumns}
\title{Subset of a data.frame's rows and and return unique values.}
\usage{
getUniqueColumns(dataFrame, columns)
}
\arguments{
\item{dataFrame}{a data.frame with the supposed format.}

\item{columns}{a vector containing the names of the columns that are to be used as ids.}
}
\value{
a data.frame with only the selected columns containing unique values.
}
\description{
This function selects a subset of a data.frame's rows and and returns unique values.
}

