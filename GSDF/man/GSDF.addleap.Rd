% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GSDF.R
\name{GSDF.addleap}
\alias{GSDF.addleap}
\title{Add fake data for February 29th.}
\usage{
GSDF.addleap(d)
}
\arguments{
\item{d}{GSDF field}
}
\value{
field with Feb 29th removed
}
\description{
Makes a fake Feb 29th in a data slab by copying Feb 28th.
 Throws error if run on a slab with Feb 29th data or without Feb 28th.
}
\details{
Often want to compare data from two years, one of which was a leap year
 one approach is to make a fake Feb 29th in the other.
Most often occurs when making anomalies.
Note that the time dimension values are just copied, so the data block
 will be sized as if there was a Feb 29th, but selecting data for Feb 29 will
 fail, and selecting data for Feb 28th will select the doubled data.
}
