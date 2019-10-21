#' Simulate data with exact correlations
#'
#' Most algorithms for simulating random normal variables of a particular correlation
#' (e.g., \eqn{\rho=0.5}) only do so \emph{on average}. This function will generate random normal
#' data with exactly the correlation specified. 
#' @param n The sample size
#' @param rho The desired correlation coefficient
#'
#' @return A matrix containing correlations between the two variables
#' @export
#' @references See \url{https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables}
#'
#' @examples
#' cor(exact_correlation(100, .5))
#' cor(exact_correlation(5, 0))
exact_correlation = function(n, rho){
  theta <- acos(rho)             # corresponding angle
  x1    <- rnorm(n, 1, 1)        # fixed given data
  x2    <- rnorm(n, 2, 0.5)      # new random data
  X     <- cbind(x1, x2)         # matrix
  Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
  
  Id   <- diag(n)                               # identity matrix
  Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
  P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
  x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
  Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
  Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
  x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector  
  return(cbind(x, x1))
}