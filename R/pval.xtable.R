##' Convert p-values into strings with inequalities.
##'
##' Often times the p values are so small they must either be expressed with bajillions of digits, or in
##' scientific notation. It is common to simply state that p<.0001, or something of the sort. This function
##' takes a vector of p-values and converts them into characters so they can be expressed with inequalities.
##'	
##' @param p.values A vector (or scalar) of p-values.
##' @param round.digits How many digits should the decimals be rounded to during display?
##' @param threshold The threshold for the inequalities. This value with then be joined with the inequality
##' via \code{paste}. (e.g., threshold = .01 becomes "<.01")
##' @param scipen Should scientific notation be used? Defaults to zero. (Note: it will not entirely suppress
##' scientific notation, but will make it so extreme it will likely never happen).
##' @aliases p.xtable p.val.xtable p.value p.values
##' @return A vector of strings yielding the formatted p-values
##' @author Dustin Fife
##' @export
##' @examples
##' p.values = runif(100,0,1)
##' pval.xtable(p.values, round.digits=2, threshold=.01)
##' pval.xtable
pval.xtable = function(p.values, round.digits=4, threshold=.0001, scipen=FALSE){
	if (!scipen){
		old = options()
		old = old$scipen
		options(scipen=999)
	}
	p.values = round(p.values, round.digits)
	p.values[p.values<threshold] = paste0("<",threshold)
	if (!scipen){
		options(scipen=old)
	}
	p.values

}