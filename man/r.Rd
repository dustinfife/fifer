\name{r}
\alias{r}
\title{Extract variable column indices}
\usage{
  r(stringA, stringB, data.names, names = F)
}
\arguments{
  \item{stringA}{the name of the first variable you wish to
  extract the column for in sequence}

  \item{stringB}{the name of the last variable you wish to
  extract the column for in sequence}

  \item{data.names}{the names of the dataset}

  \item{names}{Should the names of the variables be
  returned? Or the column indices? Defaults to FALSE.}
}
\value{
  a vector of numbers corresponding to the column names
}
\description{
  Extract the column index for a sequence of variable
  names.
}
\examples{
var.names = LETTERS[1:20]
r("C", "F", var.names)
}
\author{
  Dustin Fife
}

