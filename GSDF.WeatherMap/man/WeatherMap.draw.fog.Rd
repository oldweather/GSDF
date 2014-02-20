\name{WeatherMap.draw.fog}
\alias{WeatherMap.draw.fog}
\title{Fog of ignorance}
\usage{
  WeatherMap.draw.fog(fog, Options)
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
  Draw the fog - semi-transparent grey
}
\details{
  As it's a semi-transparent area field we need to draw in
  pixel coordinates to rule out gaps or overlaps, so use an
  image.
}

