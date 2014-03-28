\name{layout.best}
\alias{layout.best}
\title{Automatically select the layout.}
\usage{
  layout.best(n, layout = T)
}
\arguments{
  \item{n}{the number of plots}

  \item{layout}{should the fuction return a preallocated
  layout object? If \code{False}, it returns a matrix}
}
\value{
  either a matrix or a layout object
}
\description{
  Given a number of plots, automatically select the layout
}
\examples{
layout.best(6)
for (i in 1:6){
	plot(rnorm(100), rnorm(100))
}
}
\author{
  Dustin Fife
}

