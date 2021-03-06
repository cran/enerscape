% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/en_extrapolation.R
\name{en_extrapolation}
\alias{en_extrapolation}
\title{Check if model extrapolates}
\usage{
en_extrapolation(en, plot = TRUE)
}
\arguments{
\item{en}{an enerscape object.}

\item{plot}{plot areas where slope is extrapolated.}
}
\value{
A list with booleans if body size or inclines extrapolates and a
  rasterLayer for where incline extrapolates. The rasterLayer is returned
  only if extrapolations are present.
}
\description{
This check if computation of the energy landscape extrapolates from the test
set of enerscape::pontzer (2016).
}
\details{
Check if body mass or incline are outside the test range of the
  model. If slope extrapolations are detected and \code{plot = TRUE}, a plot
  of where extrapolations occur is displayed.
}
\examples{
library(raster)
library(enerscape)
data("volcano")
dem <- raster(volcano)
en <- enerscape(dem, 10, unit = "kcal", neigh = 16)
en_extrapolation(en, plot = TRUE)
}
\references{
enerscape::pontzer, H. (2016). A unified theory for the energy
  cost of legged locomotion. Biology Letters, 12(2), 20150935. \doi{
  https://doi.org/10.1098/rsbl.2015.0935}.
}
