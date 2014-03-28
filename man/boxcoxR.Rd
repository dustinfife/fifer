\name{boxcoxR}
\alias{boxcox.R}
\alias{boxcoxR}
\title{Transform data using a boxcox transformation}
\usage{
  boxcoxR(x, minval = 0.01, ...)
}
\arguments{
  \item{x}{a numerical vector}

  \item{minval}{before a transformation is performed, the
  variables must often be positive. This tells R what the
  minimum value should be. Defaults to .01.}

  \item{...}{additional parameters to be used in the model
  fitting.}
}
\description{
  This function takes a variable as input, computes the
  optimal lambda using a boxcox transformation, then
  returnes a transformed version of the variable.
}
\examples{
x = rnorm(100)^2
boxcoxR(x)
}
\author{
  Dustin Fife \email{fife.dustin@gmail.com}.
}

