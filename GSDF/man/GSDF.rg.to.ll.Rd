\name{GSDF.rg.to.ll}
\alias{GSDF.rg.to.ll}
\title{Reverse rotate lats and lons}
\usage{
  GSDF.rg.to.ll(lat, lon, pole.lat, pole.lon)
}
\arguments{
  \item{lat}{vector of latitudes in rotated pole
  (degrees).}

  \item{lon}{vector of longitudes in rotated pole
  (degrees).}

  \item{pole.lat}{latitude of pole to rotate from
  (degrees).}

  \item{pole.lon}{longitude of pole to rotate from
  (degrees).}
}
\value{
  list with components 'lat' and 'lon' - vectors of
  un-rotated lat and lon (in degrees).
}
\description{
  From rotated pole to same positions in a standard pole.
}
\details{
  Convert latitudes and longitudes to the equivalents with
  a un-rotated pole. (Formulae from UMDP S1).
}

