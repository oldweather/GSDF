\name{WeatherMap.decimate.streamlines}
\alias{WeatherMap.decimate.streamlines}
\title{Thins the set of streamlines}
\usage{
  WeatherMap.decimate.streamlines(s, Options)
}
\arguments{
  \item{s}{list of streamlines - see
  \code{WeatherMap.make.streamlines}}

  \item{Options}{list of options - see
  \code{WeatherMap.set.option}}
}
\value{
  vector of integers - indices of streamlines to be
  thinned.
}
\description{
  Streamlines clump together as they move. To keep an even
  spread we remove some of them and seed new ones. This
  function decimates a set of streamlines, removing those
  clustered most closely together.
}
\details{
  Calculate a density field with a 2d normal kernel.
  Delete each streamline with probability proportional to
  the local density of streamlines.
}

