##' Perform a z-test in R
##'
##' @description This function does the standard z-test, which tests a particular dataset against a specified value
##' @details \code{z.test} will take a vector and determine whether it differs from a chosen value specified by \code{mean}
##' @title Perform a z test
##' @param data a vector of values which will be used to compute a mean
##' @param mean the value the z-test is tested against
##' @param sd the standard deviation of the null distribution
##' @param direction either "positive", "negative", or "both" (for a two-tailed test)
##' @return a list containing the computed mean, the z-statistic, and the p-value
##' @author Dustin Fife
##' @export
##' @examples
##' k = rnorm(15, 10, 3)
##' z.test(k, mean=5, sd=3)

z.test = function(data, mean=0, sd=1, direction="both"){
	
	direction=match.arg(direction, c("both", "positive", "negative"))
	####compute the mean of the data
	mu_a = mean(data)
	z = (mu_a-mean)/(sd/sqrt(length(data)))
	if (direction=="both"){
		p = pnorm(abs(z), lower.tail=F)*2
	} else if (direction=="positive"){
		p = pnorm(z, lower.tail=F)
	} else {
		p = pnorm(z, lower.tail=T)
	}
	
	cat("mu_alternative = ", mu_a, "\nmu_null=", mean, "\nz=", round(z, digits=4), "\np=", round(p, digits=5), "\nN=", length(data))
}
