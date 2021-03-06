% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/WeatherMap.R
\name{WeatherMap.bridson.parallel}
\alias{WeatherMap.bridson.parallel}
\title{The bridson point allocation is often the rate-limiting step}
\usage{
WeatherMap.bridson.parallel(Options, previous = NULL, scale.x = NULL,
  scale.y = NULL)
}
\arguments{
\item{Options}{list of options - see \code{WeatherMap.set.option}}

\item{previous}{list with elements 'lat' and lon' - set of points to
start from. Defaults to NULL - start from scratch.}

\item{scale.x}{GSDF field with horizontal wind speeds - used to adjust
horizontal distance to allow for streamlines extending with the wind.
Defaults to NULL - no adjustment made.}

\item{scale.y}{GSDF field with vertical wind speeds - used to adjust
vertical distance to allow for streamlines extending with the wind.
Defaults to NULL - no adjustment made.}
}
\value{
list with elements 'lats' and lons'
}
\description{
Run it on n cores - this means dividing the window into 2n
vertical slices, doing the n odd slices in parallel and then
the n even slices in parallel. Avoids doing two adjacent regions
at the same time, which would over-allocate points.
}
\details{
Each vertical slice is expanded a bit horizontally to avoid
boundary artefacts when doing the calculation, but only points in
the slice are kept from the results.
}

