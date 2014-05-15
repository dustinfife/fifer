\name{clear}
\alias{clear}
\title{Clear memory of all objects}
\usage{
clear(obj = NULL, keep = T)
}
\arguments{
  \item{obj}{The object (as a string) that needs to be
  removed (or kept)}

  \item{keep}{Should \code{obj} be kept (i.e., everything
  but \code{obj} removed)? Or dropped?}

  \item{env}{The environment. Defaults to global
  environment}
}
\description{
This function is a wrapper for the command
\code{rm(list=ls())}. It's just less keystrokes.
}
\examples{
#NOTE: ONLY RUN THIS IF YOU DON'T MIND ERASING YOUR R SESSION
a = 5
ls()	### a shows up
clear()
ls()	### nothing shows up
### create objects
a=1; b=2; c=3; d=4; e=5
### remove c
clear("c", keep=F)
ls()
### remove all but a and b
clear(c("a", "b"), keep=T)
ls()
}
\author{
Dustin Fife
}

