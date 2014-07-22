##' Force a matrix to be symmetric
##'
##' @param a A matrix you wish to force to be symmetrical
##' @param lower.tri Should the upper triangle be replaced with the lower triangle?
##' @return a symmetric matrix
##' @author Dustin Fife
##' @export
##' @examples
##' a = matrix(rnorm(16), ncol=4)
##' make.symmetric(a, lower.tri=FALSE)
make.symmetric = function(a, lower.tri=TRUE){
	if (lower.tri){
		ind <- upper.tri(a)
		a[ind] <- t(a)[ind]
	} else {
		ind <- lower.tri(a)
		a[ind] <- t(a)[ind]
	}
	a	
}