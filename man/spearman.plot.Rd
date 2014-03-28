\name{spearman.plot}
\alias{spearman.plot}
\title{Spearman plot}
\usage{
  spearman.plot(x, dcol = "blue", lhist = 20,
    num.dnorm = 5 * lhist, plot.cor = TRUE, ...)
}
\arguments{
  \item{x}{a matrix with two columns}

  \item{dcol}{the color of the lines drawn for the density
  plot}

  \item{lhist}{the number of breaks in the histogram}

  \item{num.dnorm}{the number of breaks in the density
  line}

  \item{plot.cor}{logical. Should the spearman correlation
  be outputted in the plot?}

  \item{...}{arguments passed to \code{plot}}
}
\description{
  Plots the relationship between two variables using a
  Spearman Plot
}
\details{
  Often data are not normally distributed, requiring the
  use a a spearman correlation to determine their
  relationship. However, doing so makes it difficult to
  visualize the data since scatterplots of raw data present
  the data as if a pearson correlation were used. This
  function plots the ranks of the data, while labeling the
  axes according to the raw data. Consequently, it is an
  accurate representation of a spearman correlation.
}
\examples{
### generate skewed data
x = rnorm(1000)^2
y = .6*x + rnorm(1000, 0, sqrt(1-.6^2))

spearman.plot(cbind(x,y), col="red", lhist=50)
}
\author{
  Dustin Fife
}

