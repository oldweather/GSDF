\name{TWCR.get.data.dir}
\alias{TWCR.get.data.dir}
\title{TWCR get data directory}
\usage{
  TWCR.get.data.dir(version = 2)
}
\value{
  Base directory name
}
\description{
  Find local data directory - different for different
  systems
}
\details{
  It's much faster to read data from local disc than over
  openDAP, also observations and standard deviations are
  currently only available locally. But the various
  different systems on which I run this code all have
  different places to keep large data files. This function
  returns the right base directory for the system (if there
  is one).
}

