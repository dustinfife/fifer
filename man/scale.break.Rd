\name{scale.break}
\alias{scale.break}
\title{Scatterplot with a Scale Break}
\usage{
\method{scale}{break}(x, y, axis = 2, breakpos = 1, ...)
}
\arguments{
  \item{x}{The x variables to be plotted.}

  \item{y}{The y variables to be plotted.}

  \item{axis}{Which axis should have two different scales.
  Currently, only implemented for "2," which is the y
  axis.}

  \item{breakpos}{At what point should the break occur?
  Defaults to 1.}
}
\description{
With skewed data, important relationships are often scaled
so small because the graph attempts to capture the
outliers. \code{scale.break} will plot two different scales
on the same plot, shrinking the range where the outliers
lay. The break in scale is indicated with a mark.
}
\examples{
# generate correlated data
d = data.frame(mvrnorm(1000, mu=c(0,0), Sigma=matrix(c(1,.6,.6,1), nrow=2)))
names(d) = c("x","y")
## Skew Y
d$y = d$y^2
scale.break(d$x, d$y, breakpos=4)
}
\author{
Dustin Fife
}

