\name{WeatherMap.draw}
\alias{WeatherMap.draw}
\title{Draw the map}
\usage{
  WeatherMap.draw(Options = NULL, t.actual = NULL,
    t.normal = NULL, icec = NULL, mslp = NULL,
    precip = NULL, obs = NULL, uwnd = NULL, vwnd = NULL,
    land = NULL, streamlines = NULL, fog = NULL)
}
\arguments{
  \item{Options}{list of options - see
  \code{WeatherMap.set.option}}

  \item{t.actual}{GSDF field of 2m temperature}

  \item{t.normal}{GSDF field of temperature normals}

  \item{icec}{GSDF field of sea-ice concentration}

  \item{mslp}{GSDF field of mean-sea-level-pressure}

  \item{precip}{GSDF field of precipitation rate}

  \item{obs}{data frame from \code{GSDF.TWCR::TWCR.get.obs}
  or equivalent.}

  \item{uwnd}{GSDF field of zonal wind}

  \item{vwnd}{GSDF field of meridional wind}

  \item{land}{list with components 'x' and 'y' - the
  coordinates of a set of polygons bounding land regions.
  Suitable for drawing with \code{grid.polygon}. If NULL,
  will be obtained from \code{WeatherMap.get.land}.}

  \item{streamlines}{list of streamline data - see
  \code{WeatherMap.make.streamlines}}

  \item{fog}{GSDF field of fog of ignorance.}
}
\value{
  nothing - side effect only.
}
\description{
  Draw everything as specified in the options
}
\details{
  Sets up a viewport and then calls all the drawing
  functions in sequence. Main user function for actual
  drawing. The basic idea is that you set the options, load
  the data, and then call this function.
}

