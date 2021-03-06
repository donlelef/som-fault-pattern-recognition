% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rootCauseDetection.R
\name{rootCauseDetection}
\alias{rootCauseDetection}
\title{Find the equipments with the lowest yields.}
\usage{
rootCauseDetection(historyFrame, badWafers)
}
\arguments{
\item{historyFrame}{a data.frame containing four columns:
EQUIPMENT: the equipment which performed an operation
OPERATION: an action performed on a lot
LOT: a lot of wafer
QUANTITY: the number of wafers in the lot}

\item{badWafers}{a data.frame containing at least two columns: LOT, 
that is the lot of the wafer, and QUANTITY, that is the number of wafer in that lot.
It is supposed to contain all the wafers to be considered bad.}
}
\value{
a data.frame which contains, for each OPERATION and EQUIPMENT,
the result of chiTestEquipment()
}
\description{
Perform chi square test through contingency matrix
on every equipment, for each operation.
}

