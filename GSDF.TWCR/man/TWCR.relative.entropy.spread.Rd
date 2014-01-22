\name{TWCR.relative.entropy.spread}
\alias{TWCR.relative.entropy.spread}
\title{Relative Entropy spread.}
\usage{
  TWCR.relative.entropy.spread(old.mean, old.sd, new.mean,
    new.sd, old.n = 30, new.n = 56, n.sigma = 1)
}
\arguments{
  \item{old.mean}{Field of best estimates from climatology}

  \item{old.sd}{Field of climatological standard
  deviations}

  \item{new.mean}{Field of best estimates from TWCR}

  \item{new.sd}{Field of standard deviations from TWCR}

  \item{old.n}{number of years observation climatological
  mean and sigma are based on (default=30).}

  \item{new.n}{number of ensembles making up TWCR
  (default=56).}

  \item{n.sigma}{number of standard deviations to perturb
  by (default=1).}
}
\value{
  list with components upper (field of RE+n*sigma) and
  lower (field of RE-n*sigma).
}
\description{
  We only estimate the relative entropy, because we only
  have estimates of the means and standard deviations. This
  function estimates RE+-n*sigma.
}
\details{
  Calculating a standard error for RE is hard, and it's an
  asymetric distribution so an exact sigma is of limited
  use anyway. This function estimates the value by
  perturbing the input means and standard deviations by
  n*sigma.
}

