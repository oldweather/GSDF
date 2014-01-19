\name{TWCR.get.slice.at.hour}
\alias{TWCR.get.slice.at.hour}
\title{Get slice at hour.}
\usage{
  TWCR.get.slice.at.hour(variable, year, month, day, hour,
    height = NULL, opendap = NULL, version = 2,
    type = "mean")
}
\arguments{
  \item{variable}{'prmsl', 'prate', 'air.2m', 'uwnd.10m' or
  'vwnd.10m' - or any 20CR variable}

  \item{type}{- 'mean' (default), 'spread', 'normal', or
  'standard.deviation'.  Note that standard deviations are
  not available over opendap.}

  \item{height}{Height in hPa - leave NULL for monolevel}

  \item{opendap}{TRUE for network retrieval, FALSE for
  local files (faster, if you have them), NULL (default)
  will use local files if available and network otherwise.}
}
\value{
  A GSDF field with lat and long as extended dimensions
}
\description{
  Get a 2D horizontal slice of a selected variable (as a
  GSDF field) for a given hour.
}
\details{
  Interpolates to the selected hour when the data available
  are less than hourly. Interpolates to the selected height
  when the selected height is not that of a 20RC level.
}

