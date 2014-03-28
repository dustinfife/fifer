\name{rfThresh}
\alias{rfThresh}
\title{Variable Selection Using Random Forests}
\usage{
  rfThresh(formula, data, nruns = 50, silent = FALSE,
    importance = "permutation", nmin = 1, ...)
}
\arguments{
  \item{formula}{a formula, such as \code{y~x1 + x2}, where
  \code{y} is the response variable and anything following
  \code{~} are predictors.}

  \item{data}{the dataset containing the predictors and
  response.}

  \item{nruns}{How many times should random forests be run
  to compute variable importance? Defaults to 50.}

  \item{silent}{Should the algorithm talk to you?}

  \item{importance}{Either "permutation" or "gini."}

  \item{nmin}{Number of times the "minimum value" is
  multiplied to set threshold value.}
}
\value{
  The object returned has the following attributes:

  \item{variable.importance}{A sorted vector of each
  variable importance measures.}

  \item{importance.sd}{the standard deviation of variable
  importance, measured across the \code{nruns} iterations.
  }

  \item{stepwise.error}{The OOB error after each variable
  is added to the model}

  \item{response}{The response variable that was modeled.}

  \item{variables}{A vector of strings that indicate which
  variables were included in the initial model.}

  \item{nruns}{How many times the random forest was
  initially run.}

  \item{formula}{the formula used for the last model.}

  \item{data}{the dataset used to fit the model.}

  \item{oob}{the oob error of the entire model.}

  \item{time}{how long the algorithm ran for}

  \item{rfmodel}{The final model used, a randomForest
  object.}
}
\description{
  Using a set of predictors, this function uses random
  forests to select the best ones in a stepwise fashion.
}
\details{
  This function begins by predicting classes using random
  forests with all of the predictors. It repeats this 50
  times, then computes variable importance for each
  variable. Subsequently, each predictor is added
  sequentially in order of importance until the value of
  \code{pick.best} is reached.
}
\author{
  Dustin Fife
}

