% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rastertrajData.R
\name{rastertrajData}
\alias{rastertrajData}
\title{creates the data which serves as input for the "trajPlot" function.}
\usage{
rastertrajData(x, zeroabsence = "yes", unified = "yes", user_spatial = 0)
}
\arguments{
\item{x}{is the raster stack.}

\item{zeroabsence}{is a string of "yes" or "no" indicating if 0 means absence. # The unified size is the union of the locations where the category exists at any time point. The default is "yes".}

\item{unified}{is a string of "yes" or "no" indicating if the analysis involves the unified size. The default is "yes"}

\item{user_spatial}{is an integer for the user-defined spatial extent. Default is 0.}
}
\value{
The output from \code{\link{rastertrajData}}
}
\description{
creates the data which serves as input for the "trajPlot" function.
}
