\name{TWCR.get.obs}
\alias{TWCR.get.obs}
\title{TWCR get observations}
\usage{
  TWCR.get.obs(year, month, day, hour, version = 2,
    range = 0.5)
}
\arguments{
  \item{range}{Date range (in days) to retrieve
  observations from - period is specified time +- range
  days (default is 0.5 - giving 1 day's obs).}
}
\value{
  A data frame - one row for each observation.
}
\description{
  Retrieves observations from the obs. feedback (prepbufr)
  files Gets all obs in +-range days around specified date
}
\details{
  Specification of obs format is at
  http://rda.ucar.edu/datasets/ds131.1/docs/ISPD_quick_assimilated_ascii_format.pdf
  File access only - the observations feedback files are
  not online.
}
\seealso{
  \code{TWCR.get.obs.1file} get the observations for a
  specific analysis run.
}

