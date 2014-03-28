\name{make.formula}
\alias{make.formula}
\title{Convert strings to a formula}
\usage{
  make.formula(response, predictors)
}
\arguments{
  \item{response}{a single string used on the left side of
  a formula}

  \item{predictors}{a string (or a vector of strings)
  representing the predictors. Each will be separated by a
  +}
}
\value{
  a formula object
}
\description{
  Given a set of strings, convert into a formula
}
\examples{
k = data.frame(matrix(rnorm(100), ncol=5))
names(k) = LETTERS[1:5]
formula = make.formula("A", LETTERS[2:5])
formula
lm(formula, data=k)
}
\author{
  Dustin Fife
}

