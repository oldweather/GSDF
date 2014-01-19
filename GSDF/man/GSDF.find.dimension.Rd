\name{GSDF.find.dimension}
\alias{GSDF.find.dimension}
\title{Find the index of a named dimension}
\usage{
  GSDF.find.dimension(g, label)
}
\arguments{
  \item{g}{GSDF structure}

  \item{label}{text string, 'lat','lon', or 'time'}
}
\value{
  Integer number of requested dimension, or NULL if not
  present.
}
\description{
  Identify the dimension that contains 'lat', 'time' etc.
}
\details{
  If the first dimension in g is 'lat', the second 'time',
  and the third 'lon', \code{GSDF.find.dimensions(g,'lon')}
  will return 3.
}

