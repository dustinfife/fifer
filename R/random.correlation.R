##' Generate a random correlation matrix
##'
##' @title Generate a random correlation matrix
##' @param n the number of rows/dimensions of the correlation matrix 
##' @return a correlation matrix, of size \code{nxn}
##' @export
##' @imports MASS
##' @author Dustin Fife
random.correlation = function(n){
	t = mvrnorm(n,rep(0,n),diag(n))
	for (i in 1:n) {
		t[i,] = t[i,]/sqrt(t(t[i,])%*%t[i,])
	}
	t%*%t(t)
}
