##' Compute the confidence interval of a mean
##'
##' This function will compute the mean/confidence interval of a single variable, or compute the mean/CIs of levels within a factor based on an outcome variable
##'	
##' @param y The variable we wish to compute the mean of
##' @param x The grouping variable (optional). If no grouping variable is specified, the function will estimate the mean of y only. 
##' @param levels The user can specify which levels of the factor they want to compute the means of. See examples.
##' @param interval What interval is requested (expressed as a proportion). Defaults to .95. 
##' @return either a list containing the mean and CI, or a table containing the means/CIs for each level of x
##' @author Dustin Fife
##' @export
##' @examples
##' iq = rnorm(100, 100, 15)
##' group = sample(c("iq.booster.pill", "control", "iq.training"), size=100, replace=T)
##' 	### compute the mean of just iq
##' ci.mean(iq)
##' 	### compute mean for all levels of x
##' ci.mean(iq, group)
##' 	### compute mean for only the iq.booster.pill group
##' ci.mean(iq, group, levels="iq.booster.pill")
##' 	### change to a 99% CI
##' ci.mean(iq, group, levels=c("control","iq.booster.pill"), interval=.99)
ci.mean = function(y,x=NULL, levels=NULL, interval=.95){
	
	#### return error message
	if (is.character(y)){
		stop("It looks like you're trying to pass a string to this function. You need to instead pass an object (e.g., d$iq.score, rather than 'iq.score').")
	}
	
	#### convert CI to two sided
	interval = 1-interval		### .05
	interval = interval/2		### .025
	interval = 1-interval		### .975
	yn = y
	xn = x
	
	if (!is.null(levels)){
		findem = which(x %in% levels)
		yn = y[findem]
		xn = as.character(x[findem])
		### if we only have one group remaining, remove x from the equation
		if (length(unique(x))==1){
			x = NULL
		}
	}
	
	### if they provide no predictor, compute the mean of y (with 95% CI)
	if (is.null(x)){
		mean.value = mean(y, na.rm=T)
		error = qt(interval,df=length(y)-1)*sd(y, na.rm=T)/sqrt(length(y))
		ci = c(mean.value-error, mean.value+error)
		list(mean=mean.value, ci=ci)
	} else {
		tab = aggregate(yn~xn, FUN=mean, na.rm=T)	
		f = function(z, interval){qt(interval, df=length(z)-1)*sd(z, na.rm=T)/sqrt(length(z))}
		error = aggregate(yn~xn, FUN=f, interval=interval)			
		tab$lower = tab$yn-error[,ncol(error)]
		tab$upper = tab$yn+error[,ncol(error)]	
		x.name = subsetString(deparse(substitute(x)), "$",2, flexible=T)	
		y.name = subsetString(deparse(substitute(y)), "$",2, flexible=T)	
		names(tab)[1:2]	=c(x.name, paste0(y.name, " mean"))	
		return(tab)
	}
}
