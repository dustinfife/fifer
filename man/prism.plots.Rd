\name{prism.plots}
\alias{plots.prism}
\alias{prism.plots}
\alias{prismPlots}
\alias{prismplots}
\title{Plot prism-like Plots}
\usage{
  prism.plots(formula, data, centerfunc = mean,
    spreadfunc = function(x) {
        return(sd(x)/sqrt(length(x))) }, def.axis = TRUE,
    jitter.y = FALSE, add = FALSE, start = 0, ...)
}
\arguments{
  \item{formula}{a formula object with the quantitative
  variable as the response variable (e.g., Var~group).}

  \item{data}{a dataset containing the variables indicated
  in \code{formula}}

  \item{centerfunc}{what function should be used to
  indicate the center of the distribution. Defaults to
  mean.}

  \item{spreadfunc}{what function should be used to
  calculate the spread of the distribution. Defaults to sd.
  Currently, it must be a symmetrical function. Future
  implementations will have non-symmetric functions (e.g.,
  interquartile range).}

  \item{def.axis}{Logical. Should the default axes be
  used?}

  \item{jitter.y}{Logical. Should the y values be jittered
  as well?}

  \item{add}{Should the plot be added to an existing plot?}

  \item{start}{What X value should the plot start at?
  (defaults to zero)}

  \item{...}{other arguments passed to plot}
}
\description{
  Plot prism-like Plots
}
\details{
  Given a factor (e.g., group membership) and a
  quantitative variable, this function plots a
  psuedo-scatterplot of the groups on the x axis (jittered)
  and the DV on the y axis.
}
\examples{
prism.plots(count ~ spray, data = InsectSprays, centerfunc=mean)
prism.plots(count ~ spray, data = InsectSprays, centerfunc=median)
}
\author{
  Dustin Fife
}
\seealso{
  \code{\link{boxplot}}, \code{\link{densityPlotR}},
  \code{\link{plotSigBars}}
}

