\name{GSDF.is.full}
\alias{GSDF.is.full}
\title{GSDF is full field}
\usage{
  GSDF.is.full(g.grid)
}
\arguments{
  \item{g.grid}{Field to test}
}
\value{
  logical: TRUE if full, FALSE otherwise
}
\description{
  Is field lat:lon and global in scope?
}
\details{
  Interpolation can shrink a field round the edges, this is
  not OK for global fields - we want to preserve global
  coverage so identify those and treat then specially. Also
  this deals with wrap-arounds in longitude (0:360 v
  -180:180).
}

