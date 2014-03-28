\name{plotSigBars}
\alias{plotSigBars}
\title{Add significance bars to a prism plot}
\usage{
  plotSigBars(formula, data, type = c("tukey", "dunn"))
}
\arguments{
  \item{formula}{a R formula object}

  \item{data}{a dataset containing the variables in
  formula}

  \item{type}{either "tukey" or "dunn" indicating which
  multiple comparison should be used}
}
\description{
  Add significance bars to a prism plot, corrected for
  multiple comparisons either using Tukey's HSD
  (parametric), or Dunn's correction for multiple
  comparison (non-parametric).
}
\note{
  This function should probably only be used when the
  number of groups is less than four, otherwise the number
  of pairwise comparisons becomes too large to display.
}
\examples{
prism.plots(Sepal.Length ~ Species, data = iris, centerfunc=mean)
plotSigBars(Sepal.Length ~ Species, data = iris, type="tukey")
}
\author{
  Dustin Fife
}
\seealso{
  \code{\link{boxplot}}, \code{\link{densityPlotR}},
  \code{\link{prism.plots}}
}

