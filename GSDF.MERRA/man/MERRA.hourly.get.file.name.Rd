\name{MERRA.hourly.get.file.name}
\alias{MERRA.hourly.get.file.name}
\title{MERRA hourly get file name}
\usage{
  MERRA.hourly.get.file.name(variable, year, month, day,
    hour, opendap = TRUE, type = "mean")
}
\arguments{
  \item{variable}{'T2M', 'SLP', 'U10M', 'H500' - or any
  MERRA variable}

  \item{type}{must be 'mean', (no spreads, normals etc
  available from MERRA)}

  \item{opendap}{Must be TRUE - no local option currently
  supported.}
}
\value{
  A file or URI containing the requested MERRA data
}
\description{
  Get file name (URI) for MERRA variable
}
\details{
  MERRA URIs depend on the variable group and the day, this
  function calculates them from the variable name and the
  date.
}

