\name{univariate.tests}
\alias{univariate.tests}
\title{Extract p values for a data frame}
\usage{
  univariate.tests(dataframe, exclude.cols = NULL, group)
}
\arguments{
  \item{dataframe}{a data frame containing both the
  variables and the grouping variable}

  \item{exclude.cols}{a vector indicating which columns
  should not have a significance test}

  \item{group}{a string with the name of the grouping
  variable}
}
\value{
  a vector of p values
}
\description{
  Extract the p value from a univariate significance test
}
\details{
  Given a dataframe, this function predicts the specified
  categorical variable using each column in the dataset,
  one at a time. The function will automatically select
  whether to do a chi-square test, a t-test, or an ANOVA.
  See details.

  \code{univariate.tests} will look at each column in the
  dataframe, then perform a t-test, ANOVA, or chi-square
  test where the grouping variable serves as the
  independent variable. The computer will chose a
  chi-square test of one of the following three conditions
  is met: (1) the variable is a factor, (2) the variable is
  a character variable, or (3) the variable has less than
  four unique values. An ANOVA will be used if the number
  of levels of the grouping variable is greater than two.
  In all other cases, a t-test will be used.
}
\examples{
k = data.frame(cbind(ID=1:100,
				A = rnorm(100),
				B = rnorm(100),
				C = rnorm(100),
				Group = rep(1:2, times=50)))
univariate.tests(dataframe = k, exclude.cols=1, group="Group")
}
\author{
  Dustin Fife
}

