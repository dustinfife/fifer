\name{univariate.tests}
\alias{univariate.tests}
\title{Extract p values for a data frame}
\usage{
  univariate.tests(dataframe, exclude.cols = NULL,
    response)
}
\arguments{
  \item{dataframe}{a data frame containing both the
  variables and the response}

  \item{exclude.cols}{a vector indicating which columns
  should not have a significance test}

  \item{response}{a string with the name of the response
  variable}
}
\value{
  a vector of p values
}
\description{
  Extract the p value from a univariate significance test
}
\details{
  This function is used to easily extract p value when
  there are a host of predictor variables. See examples.
}
\examples{
k = data.frame(cbind(ID=1:100,
				A = rnorm(100),
				B = rnorm(100),
				C = rnorm(100),
				Group = rep(1:2, times=50)))
univariate.tests(dataframe = k, exclude.cols=1, response="Group")
}
\author{
  Dustin Fife
}

