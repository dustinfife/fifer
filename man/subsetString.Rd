\name{subsetString}
\alias{subsetString}
\title{Extract only part of a string}
\usage{
  subsetString(string, sep = " ", position = 3)
}
\arguments{
  \item{a}{vector of strings}

  \item{the}{separator that separates the parts of the
  strings}
}
\value{
  the element the user wishes to extract
}
\description{
  Extract only part of a string, given a separator Given a
  string with a separator (e.g., "Subject 001"), this
  function can be used to extract only whatever follows the
  separator (in this case, "001"). It is often used when
  data comes in with a conglomorated identifier (such as
  case-matchNumber-drawNumber-Month).
}
\examples{
barcode = c("Case-001-B", "Control-001-A", "Case-002-A")
subsetString(barcode, sep="-", position=2)
subsetString(barcode, sep="-", position=3)
}
\author{
  Dustin Fife
}

