% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findFaultPosition.R
\name{findFaultPositions}
\alias{findFaultPositions}
\title{Find the position of the faults}
\usage{
findFaultPositions(faultMap, dieWidth, dieHeight, faultValue)
}
\arguments{
\item{faultMap}{a map where elements with value \code{faultValue} represent faulty chips}

\item{dieWidth}{the width of a chip}

\item{dieHeight}{the height of a chip}

\item{faultValue}{the value that indicates a fault on the map}
}
\value{
a matrix containing faulty chip (x, y) position.
}
\description{
Find the positions of the faults in the wafer. A chip is assumed to be represented as a point placed in the
middle of its spatial extension, ie: if a chip's dimensions are 2x1, it is represent
as a poin in (1;0.5). This funcion returns the x and the y axes of the wafer, one point
is placed for every chip.
}

