##' Simulate multivariate normal data
##'
##' Simulate multivariate normal data
##'	
##' This function provides an easy way to simulate random multivariate normal data. 
##' @param cor.matrix Either a scalar (indicating the correlation between two variables) or a symmetrical matrix for a variance/covariance matrix
##' @param sds The standard deviation of the variables
##' @param means The means of the variables
##' @param names The names of the variables
##' @param n The desired sample size
##' @return A data frame containing the simulated data
##' @author Dustin Fife
##' @export
##' @examples
##' generate.data(cor.matrix=.2, sds=c(10, 100), means=c(30, 1000), c("A", "B"), 200)
generate.data = function(cor.matrix, sds=c(1,1), means=c(0,0), names=c("y", "x"), n=100){
	if (length(cor.matrix)==1){
		cor.matrix = matrix(c(1,cor.matrix, cor.matrix, 1), nrow=2)
	}
	cov.mat = cor2cov(cor.matrix, sds)
	d = data.frame(mvrnorm(n, mu=means, Sigma=cov.mat))
	names(d) = names
	d
}