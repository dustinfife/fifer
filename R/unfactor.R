##' Convert a factor to a character (or number)
##'
##' Analogous to the function \code{\link{factor}}, \code{unfactor}	will convert a factor to a character or integer
##'	
##' @param x The vector of factors you wish to replace
##' @param levels A numeric vector of the values that will replace the factors
##' @param labels A character vector of the values to be replaced
##' @param numeric Should the labels be returned (i.e., characters)? Or the numbers?
##' @seealso \code{\link{factor}}
##' @return A numeric or character vector
##' @author Dustin Fife
##' @export
##' @examples
##'		#### create sample of male/female participants (at random)
##'	x = as.factor(sample(c("Male", "Female"), 12, replace=TRUE))
##'	unfactor(x, levels=c(0,1), labels=c("Male", "Female"))
unfactor = function(x, levels, labels, numeric=FALSE){
	
	if ((length(labels) != length(levels)) | (length(unique(x)) != length(levels))){
		stop("There's a mismatch in the number of levels of either x, the variable levels, or the varable labels.")
	}
	
	if (numeric){
		return(as.character(x))
	} else {
		x = as.character(x)
		xnew = as.numeric(levels)[match(x, labels)]
		xnew
	}
}