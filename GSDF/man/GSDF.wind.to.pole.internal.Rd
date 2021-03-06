% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GSDF.R
\name{GSDF.wind.to.pole.internal}
\alias{GSDF.wind.to.pole.internal}
\title{Rotate winds - internal detail}
\usage{
GSDF.wind.to.pole.internal(u, v, lat.orig, lon.orig, lat.new, lon.new, pole.lat,
  pole.lon = 180)
}
\arguments{
\item{u}{vector of zonal wind speeds in source pole.}

\item{v}{vector of meridional wind speeds in source pole.}

\item{lat.orig}{vector of latitudes of wind vectors in source pole (degrees).}

\item{lon.orig}{vector of longitudes of wind vectors in source pole (degrees).}

\item{lat.new}{vector of latitudes of wind vectors in target pole (degrees).}

\item{lon.new}{vector of longitudes of wind vectors in target pole (degrees).}

\item{pole.lat}{latitude of pole to rotate to (degrees).}

\item{pole.lon}{longitude of pole to rotate to (degrees).}
}
\value{
list with components 'u' and 'v' - vectors of
  rotated zonal and meridional wind speeds.
}
\description{
From u & v one pole to same in a different pole.
}
\details{
(Formulae from UMDP S1). Works for any vector field, not just winds.
}
\seealso{
\code{\link{GSDF.ll.to.rg}} and \code{\link{GSDF.rg.to.ll}}.
}
