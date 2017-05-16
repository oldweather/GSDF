\name{GFS.get.member.slice.at.hour}
\alias{GFS.get.member.slice.at.hour}
\title{Get slice at hour.}
\usage{
  GFS.get.member.slice.at.hour(variable, year, month, day,
    hour, height = NULL, opendap = TRUE, member = 1,
    lead = 0)
}
\arguments{
  \item{variable}{'tmp2m', 'prmslmsl', 'ugrd10m', 'tmpprs'
  - or any GFS variable}

  \item{(year,month,day,hour)}{Time at which the forecast
  was made.}

  \item{lead}{number of hours ahead of forecast time
  (0-259, default=0)}

  \item{type}{must be 'member', 'mean', or 'spread' (no
  normals or sds from GFS)}

  \item{member}{The member to use' - integer in the range
  0-20 (default=0, mean and sd use all members)}

  \item{height}{Height in hPa - leave NULL for monolevel}

  \item{opendap}{Must be TRUE - no local option currently
  supported.}
}
\value{
  A GSDF field with lat and long as extended dimensions
}
\description{
  Get a 2D horizontal slice of a selected variable (as a
  GSDF field) for a given hour.
}
\details{
  Interpolates to the selected hour when the data available
  are less than 6-hourly. Interpolates to the selected
  height when the selected height is not that of a GFS
  level.
}

