\name{rfPred}
\alias{rfPred}
\title{Variable selection in Random Forest}
\usage{
  rfPred(object, importance = "permutation",
    nfor.pred = 25, nmj = 1, ...)
}
\arguments{
  \item{object}{an object returned from
  \code{\link{rfInterp}}}

  \item{importance}{what importance measure should be used?
  Either "permutation" or "gini."}

  \item{nfor.pred}{number of forests to grow}

  \item{nmj}{i don't know}
}
\value{
  \item{varselect.pred}{The variables selected for
  Prediction (sorted)}

  \item{err.interp}{The error at each stage of the stepwise
  variable inclusion.}

  \item{mean.jump}{The threshold for variable inclusion.}

  \item{stepwise.error}{The OOB error rate at each
  iteration of the stepwise procedure.}

  \item{num.varselect.pred}{The number of variables
  selected for prediction.}

  \item{comput.time}{Computation time of the procedure.}

  \item{model}{The final model, either a
  \code{randomForest} or \code{cforest} object.}
}
\description{
  Variable selection for prediction purposes using Random
  Forest
}
\examples{
\dontrun{data(iris);
data = iris;
formula = as.formula("Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")
thresh = rfThresh(formula, data=iris, nruns=2, importance="permutation");
interp = rfInterp(thresh, importance="permutation");
predic = rfPred(interp, importance="gini")
predic}
}
\author{
  Dustin Fife
}

