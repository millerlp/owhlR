% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/millibarToSeawater.R
\name{millibarToSeawater}
\alias{millibarToSeawater}
\title{Convert pressure data in millibar to seawater depth in meters}
\usage{
millibarToSeawater(x, latitude = NULL)
}
\arguments{
\item{x}{A vector of pressure data, units of millibar}

\item{latitude}{The latitude at which the seawater pressure data were
collected.}
}
\value{
A vector of seawater depth in meters
}
\description{
Convert pressure data in millibar to seawater depth in meters
}
\details{
Convert water pressure data, in units of millibar, to the
equivalent depth (or elevation above the pressure sensor if you
prefer that perspective) of seawater in meters.
}
\examples{
millibarToSeawater(2039.604, latitude = 33.72) # Should return 20.25 (meters)
}
