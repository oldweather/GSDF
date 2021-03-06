% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/WeatherMap.R
\name{WeatherMap.draw.pressure}
\alias{WeatherMap.draw.pressure}
\title{Draw Pressure Contours}
\usage{
WeatherMap.draw.pressure(mslp, Options)
}
\arguments{
\item{mslp}{GSDF field of pressure (typically in Pa)}

\item{Options}{list of options - see \code{WeatherMap.set.option}}
}
\value{
nothing - side effect only.
}
\description{
Draw a pressure field with semi-transparent monochrome contours
}
\details{
The idea is to mark high and low pressure regions, but not to
 generate much visual clutter. So this is a standard monochrome
 contour plot, except that contours close to the base value are transparent
}

