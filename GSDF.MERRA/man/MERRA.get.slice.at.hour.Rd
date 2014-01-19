\name{MERRA.get.slice.at.hour}
\alias{MERRA.get.slice.at.hour}
\title{Get slice at hour.}
\usage{
  MERRA.get.slice.at.hour(variable, year, month, day, hour,
    height = NULL, opendap = TRUE, type = "mean")
}
\arguments{
  \item{variable}{'T2M', 'SLP', 'U10M', 'H500' - or any
  MERRA variable}

  \item{type}{must be 'mean', (no spreads, normals etc
  available from MERRA)}

  \item{height}{Height in hPa - leave NULL for monolevel}

  \item{opendap}{Must be TRUE - no local option currently
  supported.}
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
  when the selected height is not that of a MERRA level.
}

