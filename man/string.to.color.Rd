\name{string.to.color}
\alias{string.to.color}
\title{Convert between strings to colors}
\usage{
  string.to.color(string, colors = NULL)
}
\arguments{
  \item{string}{a vector of strings representing groups.}

  \item{colors}{a vector of colors, one for each unique
  element in \code{string}.}
}
\value{
  a vector of colors, one for each element in \code{string}
}
\description{
  Automatically convert a vector of strings into a color
  for easy plotting
}
\examples{
groups = sample(LETTERS[1:5], size=100, replace=TRUE)
plot(rnorm(100), rnorm(100), col=string.to.color(groups))
}
\author{
  Dustin Fife
}

