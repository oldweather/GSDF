\name{WeatherMap.draw.precipitation}
\alias{WeatherMap.draw.precipitation}
\title{Precipitation}
\usage{
  WeatherMap.draw.precipitation(precip, Options)
}
\arguments{
  \item{fog}{GSDF field of fog thickness (0-1)}

  \item{Options}{list of options - see
  \code{WeatherMap.set.option}}
}
\value{
  nothing - side effect only.
}
\description{
  Draw the precipitation rate - semi-transparent black
}
\details{
  As it's a semi-transparent area field we need to draw in
  pixel coordinates to rule out gaps or overlaps, so use an
  image.
}

