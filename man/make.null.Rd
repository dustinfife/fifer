\name{make.null}
\alias{make.null}
\title{Drop or keep variables in a dataset}
\usage{
  make.null(..., data, keep = FALSE)
}
\arguments{
  \item{...}{objects (either vectors of strings, vectors of
  numbers, or both)}

  \item{data}{the dataset you're trying to eliminate (or
  keep) variables from}

  \item{keep}{should the variables be kept or dropped?}
}
\value{
  a dataset, having the variables of interest only
}
\description{
  Given a set of variable names (or integers), remove (or
  keep) these columns from a dataset
}
\examples{
data = data.frame(matrix(rnorm(100), ncol=5))
names(data) = LETTERS[1:5]

#### extract only the classification
data(iris)
new.data = make.null(c("Sepal.Length"), r("Sepal.Width", "Petal.Width", names(iris)), data=iris)

#### extract all but the classification
new.data2 = make.null(c("Sepal.Length"), r("Sepal.Width", "Petal.Width", names(iris)), data=iris, keep=TRUE)
}
\author{
  Dustin Fife
}

