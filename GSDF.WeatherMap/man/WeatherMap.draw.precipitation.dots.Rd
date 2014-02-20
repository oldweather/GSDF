\name{WeatherMap.draw.precipitation.dots}
\alias{WeatherMap.draw.precipitation.dots}
\title{Draw precipitation - retired - image is too messy}
\usage{
  WeatherMap.draw.precipitation.dots(lat, lon, precip,
    t.actual, Options)
}
\arguments{
  \item{lat}{vector of latitutes}

  \item{lon}{vector of longitudes}

  \item{precip}{GSDF field of precipitation rate (default
  TWCR units)}

  \item{t.actual}{GSDF field of 2m temperature (to split
  rain and snow)}

  \item{Options}{list of options - see
  \code{WeatherMap.set.option}}
}
\value{
  nothing - side effect only.
}
\description{
  Draw precipitation at a set of points.
}
\details{
  Precipitation is drawn as clouds of dots (details
  controled by Options).  Actually we plot
  sqrt(precipitation) otherwise the range is too large.
}

