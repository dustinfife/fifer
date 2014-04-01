\name{contents}
\alias{contents}
\title{View the contents (functions) of a package}
\usage{
  contents(x)
}
\arguments{
  \item{x}{a string containing the name of the package.
  Defaults to "fifer".}
}
\value{
  a string of all the functions contained within that
  package.
}
\description{
  View the contents (functions) of a package
}
\details{
  It's easy to forget what functions are contained within a
  package. \code{contents} will return all the functions
  within a package
}
\examples{
## see what's in fifer
contents()
## see what's in MASS
contents("MASS")
}
\author{
  Dustin Fife
}

