% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/GSDF.ERA4v.R
\name{ERA4v.get.slice.at.hour}
\alias{ERA4v.get.slice.at.hour}
\title{Get slice at hour.}
\usage{
ERA4v.get.slice.at.hour(variable, year, month, day, hour, height = NULL,
  fc.init = NULL)
}
\arguments{
\item{variable}{'prmsl', 'prate', 'air.2m', 'uwnd.10m' or 'vwnd.10m' - or any supported variable.}

\item{height}{Height in hPa - leave NULL for monolevel}

\item{fc.init}{- which forecast initialisation hour to use: 9, 21, blend (gives combination
of both with a smooth transition), or NULL (default, uses whichever has shortest lag time).}
}
\value{
A GSDF field with lat and long as extended dimensions
}
\description{
Get a 2D horizontal slice of a selected variable (as a GSDF field) for a given hour.
}
\details{
Interpolates to the selected hour when the data available are less than hourly.
Interpolates to the selected height when the selected height is not that of a ERA5 level.
}

