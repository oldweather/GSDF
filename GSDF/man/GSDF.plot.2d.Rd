\name{GSDF.plot.2d}
\alias{GSDF.plot.2d}
\title{Map a field with only 2 dimesions}
\usage{
  GSDF.plot.2d(g, dimensions = NULL, palette = "diverging",
    ncols = 17, levels = NULL, draw = TRUE,
    continents = NULL, y.range = NULL, y.scale = NULL,
    y.label = NULL, x.range = NULL, x.scale = NULL,
    x.label = NULL, contour = FALSE, region = TRUE,
    pretty = FALSE, aspect = NULL)
}
\arguments{
  \item{g}{GSDF (with 2 extended dimensions)}

  \item{dimensions}{2-element vector; indexes of the 2
  extended dimensions first will be the x axis, second the
  y.}

  \item{palette}{'diverging', 'sequential', or 'greyscale'}

  \item{ncols}{Number of colours to use (overridden by
  levels)}

  \item{levels}{vector giving colour boundaries -
  seq(-10,10,1) will paint everything between -10 and -9
  one colour, -9 and -8 the next and so on.}

  \item{draw}{If true, draw the map; if false return a grob
  (to be 'print'ed later).}

  \item{continents}{Draw continent outlines if true, not if
  false. If NULL (default) draw only if plot is long::lat.}

  \item{y.range}{Range to show on y axis - units of
  relevant dimension. If NULL (default) use range of data.}

  \item{x.range}{Range to show on x axis - units of
  relevant dimension. If NULL (default) use range of data.}

  \item{y.scale}{Tic positions on y axis - units of
  relevant dimension. If NULL (default) generate
  automatically.}

  \item{x.scale}{Tic positions on x axis - units of
  relevant dimension. If NULL (default) generate
  automatically.}

  \item{y.label}{Text label for y axis. If NULL (default)
  generate automatically.}

  \item{x.label}{Text label for x axis. If NULL (default)
  generate automatically.}

  \item{contour}{Draw contour plot.}

  \item{region}{Draw image map.}

  \item{aspect}{Control aspect ratio - see ?contourplot. If
  NULL (default) set to 'iso' for lat::lon and lon::lat and
  to 'fill' otherwise.}
}
\value{
  Grob containing map if draw=FALSE, nothing otherwise.
}
\description{
  Make an imagemap or contourplot of a 2-dimensional GSDF
  field
}
\details{
  Plots a 2-dimensional slice - if it's a long::lat plot
  also show continent outlines by default. Requires the
  field to have exactly two dimensions with length greater
  than 1. (So a 10x20 array is fine, as is 20x1x200x1, but
  not 10x20x5).
}

