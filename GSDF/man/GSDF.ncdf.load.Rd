\name{GSDF.ncdf.load}
\alias{GSDF.ncdf.load}
\title{Load a GSDF field from a netCDF file or URL}
\usage{
  GSDF.ncdf.load(file, variable, lat.range = NULL,
    lon.range = NULL, height.range = NULL,
    time.range = NULL, custom.range = NULL,
    default.calendar = "gregorian", lat.name = NULL,
    lon.name = NULL, height.name = NULL, time.name = NULL)
}
\arguments{
  \item{file}{Text name of file or URI.}

  \item{variable}{Text name of variable (in file).}

  \item{lat.range}{Two-element vector with max and min
  latitudes to retrieve.  Leave NULL only if variable has
  no latitude dimension.}

  \item{lon.range}{Two-element vector with max and min
  longitudes to retrieve.  Leave NULL only if variable has
  no longitude dimension.}

  \item{height.range}{Two-element vector with max and min
  heights to retrieve.  Leave NULL only if variable has no
  height dimension.}

  \item{time.range}{Two-element vector of chron with max
  and min times to retrieve.  Leave NULL only if variable
  has no time dimension.}

  \item{default.calendar}{What calendar does file use (if
  not specified in file)?  Supported values are
  'gregorian', 'proleptic_gregorian', '365_day', and
  '360_day'.}

  \item{lat.name}{Text name used by file as label of
  latitude variable. If NULL, will try to guess it.}

  \item{lon.name}{Text name used by file as label of
  longitude variable. If NULL, will try to guess it.}

  \item{height.name}{Text name used by file as label of
  height variable. If NULL, will try to guess it.}

  \item{time.name}{Text name used by file as label of time
  variable. If NULL, will try to guess it.}
}
\value{
  GSDF field with selected subset of file data.
}
\description{
  Loads a specified hyperslab (range of lat, lon, height
  and time) from a NetCDF file (or an openDAP server)
}
\details{
  This works only for some netCDF files (netCDF is a very
  flexible file format and it's necessary to make
  assumptions about how the data is stored); currently
  known to work are the 20CR and MERRA reanalyses. Writing
  is not yet supported.
}

