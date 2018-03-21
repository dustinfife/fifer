##' Perform a regression/correlation analysis
##'
##' Perform a regression/correlation analysis in such a way that it focuses on statistics of interest (graphics first, then estimates, then p-values)
##'	
##' This function first shows a plot of the relationship, along with residual plots. When requested, it also outputs estimates and p-values
##' @param y The outcome variable of interest
##' @param x The predictor variable of interest
##' @seealso \code{\link{MASS}}
##' @author Dustin Fife
##' @export
##' @examples
##' x = rnorm(100, 0, 1)
##' y = .8*x + rnorm(100, 0, sqrt(1-.8^2))
##' reg.mod = regression(x,y)
##' estimates(reg.mod)
##' plot(reg.mod)
##' report(reg.mod)
regression = function(y,x){
	
	##### check if y/x are same length
	if (length(x) != length(y)) {
        stop(paste0(deparse(substitute(x)), " and ", deparse(substitute(y))), 
            " need to be the same length!")
    }

	##### combine into gg-friendly dataframe
	m = data.frame(y = y, x = x)
	
	#### handle missing values
	miss = which(is.na(m$y) | is.na(m$x))
	if (length(miss) > 0) {
		warning("Missing values are being removed from the dataset.")
		m = m[-miss, ]
	}


	n = length(y)

	#### name variables
	x.name = deparse(substitute(x))
	y.name = deparse(substitute(y))    

	#### perform the regression (compute the estimates)
	mod = lm(y~x, data=m)
	
	#### output fitted and residuals
	m$fitted = fitted(mod)
	m$residuals = residuals(mod)

	#### make a table of estimates/effect sizes
	t.crit = qt(.975, df=n-2)
	s.e. = summary(mod)$coefficients[,"Std. Error"]
	estimates = data.frame(Estimate=coef(mod), 
							Lower=coef(mod) - t.crit*s.e.,
							Upper = coef(mod) + t.crit*s.e.)
	row.names(estimates) = c("Intercept", paste0("Slope (", subsetString(x.name, "$", 2, flexible=T), ")"))
	sigma = summary(mod)$sigma	
	
	##### compute r squared and CI
							
	r.squared = summary(mod)$r.squared
	se.r = sqrt((4*r.squared*(1-r.squared)^2*(n-1-1)^2)/((n^2-1)*(n+3)))		### from cohen, cohen, west, aiken, page 88
	r.squared = c(r.squared, r.squared-t.crit*se.r, r.squared+t.crit*se.r)
	r.squared = round(r.squared, digits=4)
	
	##### make a report of statistical significance
	correl = cor(m)[1,2]
	p.value = summary(mod)$coef[2,4]
	p.report = ifelse(p.value<.001, "p < 0.001", paste0("p = ", round(p.value, digits=3)))
	report = paste0("r(", n-2, ") = ", round(correl, digits=3), ", ", p.report)
							
	output = list('Estimates' = estimates, 'Sigma' = sigma, 'R.squared' = r.squared, 'r' = correl, "Report" = report,
				"lm.object" = mod, "data" = m, "x.name" = x.name, "y.name" = y.name)
	attr(output, "class") = "regression"
	return(output)
}

#' Report Estimates (effect sizes and parameters)
#'
#' Report object Estimates
#' @aliases estimates.regression estimates estimates.ttest estimates.default
#' @param object a object
#' @export
estimates = function(object,...){
	UseMethod("estimates")
}

#' Output APA style statistical significance from an object 
#'
#' Output APA style statistical significance from an object
#' @aliases report.regression report report.ttest report.default
#' @param object a object
#' @export
report = function(object,...){
	UseMethod("report")
}

#' Output APA style statistical significance from an object 
#'
#' Output APA style statistical significance from an object
#' @aliases report.regression report report.ttest report.default
#' @param object a object
#' @export
estimates.default = function(object){
	out = coef(object)
	class(out) = "estimates"
	out
}

#' Output APA style statistical significance from an object 
#'
#' Output APA style statistical significance from an object
#' @aliases report.regression report report.ttest report.default
#' @param object a object
#' @export
report.default = function(object){
	out = coef(object)
	class(out) = "report"
	out
}

#' Report regression object Estimates (effect sizes and parameters)
#'
#' Report regression object Estimates
#' @aliases estimates.regression estimates
#' @param object a regression object
#' @export
estimates.regression = function(object){
	file.name = deparse(substitute(object))
	cat(paste("R squared:\n", round(object$R.squared[1], digits=3), " (", round(object$R.squared[2], digits=2),", ", round(object$R.squared[3], digits=2),")\n\nParameter Estimates:\n",sep=""))
	print(object$Estimates)
	cat(paste("\n\nr = ", round(object$r, digits=4), "\nsigma = ", round(object$Sigma, digits=4)))
}



#' Output APA style statistical significance from a regression object
#'
#' Output APA style statistical significance from a regression object
#' @aliases report.regression report report.default
#' @param object a regression object
#' @export
report.regression = function(object){
	print(object$Report)
}


#' Print regression Summary
#'
#' Print regression Summary
#' @aliases regression.ttest
#' @param x a regression object
#' @param ... ignored
#' @export
print.regression = function(x,...){
	cat(paste("\n\n Objects within this object:\n"))
	print(names(x))	
}



#' Plot regression Summary
#'
#' Plot regression Summary
#' @aliases plot plot.regression
#' @param x a regression object
#' @param y igorned
#' @param ... other parameters passed to plot
#' @import cowplot
#' @export
plot.regression = function(x, ...){
	m = x$data
	
	x.name = subsetString(x$x.name, "$", 2, T)
	y.name = subsetString(x$y.name, "$", 2, T)
	
	##### do a plot
	scatter = ggplot(data=m, aes(x=x, y=y)) + geom_point(alpha = .35, size=.75) + geom_smooth(method="loess") + 
		theme_bw() + labs(x=x.name, y=y.name, title="Scatterplot of Raw Data")

	histo = ggplot(data=m, aes(x=residuals)) + geom_histogram(fill='lightgray', col='black') + theme_bw() + labs(x=x.name, title="Histogram of Residuals")

	res.dep = ggplot(data=m, aes(x=fitted, y=residuals)) + geom_point(alpha = .35, size=.75) + geom_smooth(method="loess") + 
		theme_bw() + labs(x="Fitted", y="Residuals", title="Residual Dependence Plot")

	##### and a SL plot
	sl = ggplot(data=m, aes(y=residuals, x=x)) + geom_point(alpha=.35, size=.75) + geom_smooth(method="loess") +
			theme_bw() + labs(x=x.name, y="Absolute Value of Residuals", title="S-L Plot")
	
	##### put into a single plot
	require(cowplot)
	plot_grid(scatter, histo, res.dep, sl, nrow=2)

}