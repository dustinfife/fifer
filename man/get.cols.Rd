\name{get.cols}
\alias{get.cols}
\title{Extract column index}
\usage{
  get.cols(string, names)
}
\arguments{
  \item{string}{the name of a variable (string)}

  \item{names}{the names of the variables for which you
  wish to extract the column}
}
\value{
  an integer corresponding to the column number
}
\description{
  Extract the column index for a given variable name
}
\examples{
string = c("A", "B", "Q")
names = LETTERS[1:10]
get.cols(c("A", "B", "D"), LETTERS[1:10])  ### works
#get.cols(c("A", "B", "Z"), LETTERS[1:10])  ### doesn't work
}
\author{
  Dustin Fife
}

