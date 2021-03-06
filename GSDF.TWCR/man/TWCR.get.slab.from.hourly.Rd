% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GSDF.TWCR.R
\name{TWCR.get.slab.from.hourly}
\alias{TWCR.get.slab.from.hourly}
\title{Extract a hyperslab of data.}
\usage{
TWCR.get.slab.from.hourly(variable, date.range, height.range = NULL,
  lat.range = c(-90, 90), lon.range = c(0, 360), opendap = NULL,
  version = 2, type = "mean")
}
\arguments{
\item{variable}{20CR variable name, only 2d variables will work.}

\item{date.range}{A pair of text date strings, e.g. c('1981-02-05:12','1981-03-27:18') - inclusive.}

\item{height.range}{Bottom and top heights in hPa - leave NULL for monolevel}

\item{lat.range}{Min and max latitude in degrees - defaults to c(-90,90)}

\item{lon.range}{Min and max longitude in degrees - defaults to c(0,360)}

\item{opendap}{TRUE for network retrieval, FALSE for local files (faster, if you have them), NULL (default)
will use local files if available and network otherwise.}

\item{version}{20CR version number - defaults to 2.}

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
Time must not span year boundaries (I.e. all in one file).
}
