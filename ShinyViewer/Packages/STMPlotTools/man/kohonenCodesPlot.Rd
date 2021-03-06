% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kohonenCodesPlot.R
\name{kohonenCodesPlot}
\alias{kohonenCodesPlot}
\title{Plot a bidimensional matrix}
\usage{
kohonenCodesPlot(kohonenObject, dieWidth, dieHeight, waferRay,
  colorMap = rev(brewer.pal(11, "RdYlBu")), grid, ...)
}
\arguments{
\item{kohonenObject}{the kohonen class object to plot}

\item{dieWidth}{the width of the single die}

\item{dieHeight}{the heigth of the single die}

\item{waferRay}{the ray of the wafer}

\item{colorMap}{the colors to be used in the plot. Must be a valid argument for "col".}

\item{grid}{a list where the x element represents the array of the x coordinates
of the chips' centers on the x axes, while the y element represents the array of the y coordinates
of the chips' centers on the x axes.}

\item{...}{other arguments to pass to the lower functions}
}
\value{
nothing: just perform the plot
}
\description{
Plot a series of subplots corresponding to the different codebooks stored in the
kohonen object.
}
\seealso{
plot.kohonen
}

