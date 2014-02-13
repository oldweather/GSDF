\name{GFS.hourly.get.file.name}
\alias{GFS.hourly.get.file.name}
\title{GFS hourly get file name}
\usage{
  GFS.hourly.get.file.name(variable, year, month, day,
    hour, opendap = TRUE, type = "member", member = 0)
}
\arguments{
  \item{variable}{'tmp2m', 'prmslmsl', 'ugrd10m', 'tmpprs'
  - or any GFS variable}

  \item{type}{must be 'member' (everthing else derived from
  members).}

  \item{member}{The member to use' - integer in the range
  0-20.}

  \item{opendap}{Must be TRUE - no local option currently
  supported.}
}
\value{
  A URI containing the requested GFS data
}
\description{
  Get file name (URI) for GFS variable
}
\details{
  GFS URIs depend on the time of the run, this function
  calculates them from the variable name and the run date.
}

