% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GSDF.ERA20C.R
\name{ERA20C.get.slab.from.hourly}
\alias{ERA20C.get.slab.from.hourly}
\title{Extract a hyperslab of data.}
\usage{
ERA20C.get.slab.from.hourly(variable, date.range, height.range = NULL,
  lat.range = c(-90, 90), lon.range = c(0, 360), type = "mean")
}
\arguments{
\item{variable}{ERA20C variable name.}

\item{date.range}{A pair of text date strings, e.g. c('1981-02-05:12','1981-03-27:18') - inclusive.}

\item{height.range}{Bottom and top heights in hPa - leave NULL for monolevel}

\item{lat.range}{Min and max latitude in degrees - defaults to c(-90,90)}

\item{lon.range}{Min and max longitude in degrees - defaults to c(0,360)}

\item{type}{- 'mean' (default), 'spread', 'normal', or 'standard.deviation'.
Note that standard deviations are not available over opendap.}
}
\value{
A GSDF field with the selected multidimensional data
}
\description{
Up to 4d (lat, lon, height, time).
}
\details{
This is an excellent way to generate out of memory errors - use with caution.
}
