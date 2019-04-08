##' Perform a one-way anova
##'
##' This function performs a one-way anova, but does so in a way that adheres to the 7 steps of data analysis (it reports residuals/effect sizes/parameter estimates before showing significance). 
##'	
##' The validity of an anova relies on basic statistical assumptions (normality, homoskedasiticity, and independence). Furthermore, statistical significance can easily be
##' conflated with practical significance. This function is simply a wrapper for r's native \code{\link{anova}} function, but outputs the results in such a way that encourages
##' the user to focus on estimation as model appropriateness. 
##' @param y A vector containing the Dependent variable scores
##' @param x A vector containing the group categories
##' @seealso \code{\link{anova}}
##' @author Dustin Fife
##' @export
##' @examples
#' x = rep(1:3, times=30)
#' y = 10 + 5*x + rnorm(90, 0, 8)
#' a = fifer.anova(y,group=x)
#' plot(a)
#' estimates(a)
#' report(a)
fifer.anova = function(y, group){

	x.name = deparse(substitute(group))
	y.name = deparse(substitute(y))	
	
	if (!is.factor(group)){
		warning(paste0("I'm converting ", x.name, " to a factor"))
	}
	group = as.factor(group)

	m = data.frame(y=y, group= group)		
	miss = which(is.na(m$y) | is.na(m$group))
	if (length(miss)>0){
		m = m[-miss,]
	}		

	n = length(y)
	k = length(unique(group))
	

	
	##### perform regression
	test = lm(y~group, data=m)
		
	##### compute effect size + CI
	eta.sq = summary(test)$r.squared
	se = sqrt((((4*eta.sq)*1-eta.sq)^2*(n-k-1)^2)/((n^2-1)*(n+3)))
	t = qt(.025, n-1, lower.tail=F)
	eta.sq = c(eta.sq, eta.sq-t*se, eta.sq+t*se)
	names(eta.sq) = c("eta.squared", "lower", "upper")		


	##### estimates
	means =	ci.mean(m$y, m$group)
	names(means) = c("Group", "Mean", "Lower 95% CI", "Upper 95% CI")
	differences = data.frame(TukeyHSD(aov(test))$group)
	names(differences) = c("Difference", "Lower 95% CI", "Upper 95% CI", "p (Tukey's HSD)")

	#### sigma
	sigma = summary(test)$sigma
	
	
	#### output information for plotting:
	fitted = fitted(test)
	m$residuals = residuals(test)
	m$abs.resids = abs(m$residuals)

	##### prepare report
	p.report = ifelse(anova(test)[1,"Pr(>F)"]<.001, "p < 0.001", paste0("p = ", round(anova(test)[1,"Pr(>F)"], digits=3)))
	report = paste0("F(", anova(test)$Df[1], ", ", anova(test)$Df[2], ") = ", round(anova(test)[1,"F value"], digits=2), ", ", p.report)
	output = list('eta.squared' = eta.sq, 'differences'=differences, 
			'means' = means,
			'sigma' = sigma,
			'report' = report, lm.object = test, 'data' = m, 'x' = x.name, 'y' = y.name)
	attr(output, "class") = "fifer.anova"
	return(output)
}




#' Report fifer.anova Estimates (effect sizes and parameters)
#'
#' Report fifer.anova object Estimates
#' @aliases fifer.anova.estimates
#' @param object a fifer.anova object
#' @export
estimates.fifer.anova = function(object){
	file.name = deparse(substitute(object))
	cat(paste("Eta Squared:\n", round(object$eta.squared[1], digits=3), " (", round(object$eta.squared[2], digits=2),", ", round(object$eta.squared[3], digits=2),
			")\n\nGroup Means:\n",sep=""))
	print(object$means)
	cat(paste0("\n\n Mean Differences:\n"))
	print(object$differences[,-4])
}


#' Output APA style statistical significance from a fifer.anova object
#'
#' Output APA style statistical significance from a fifer.anova object
#' @aliases report.fifer.anova report report.default
#' @param object a fifer.anova object
#' @export
report.fifer.anova = function(object){
	cat(object$report)
	cat(paste0("\n\n"))
	tab = anova(object$lm.object)
	names(tab) = c("DF", "SS", "MS", "F", "p")
	row.names(tab)[2] = "Error"
	print(tab)

}




#' Plot fifer.anova Summary
#'
#' Plot fifer.anova Summary
#' @aliases plot plot.fifer.anova
#' @param x a fifer.anova object
#' @param y igorned
#' @param ... other parameters passed to plot
#' @importFrom cowplot plot_grid
#' @export
plot.fifer.anova = function(x, ...){
	m = x$data
	
	x.name = x$x.name
	y.name = x$y.name
	
	##### do a plot
	t.test = ggplot(data=m, aes(x=group, y=y)) + geom_jitter(alpha = .15, width=.05, size=.75) + stat_summary(fun.y='median', geom='point', size=2, color='red') + 
		stat_summary(aes(x=group, y=y), geom='errorbar', fun.ymin=function(z) {quantile(z, .25)}, fun.ymax = function(z) {quantile(z, .75)}, fun.y=median, color='red', width=.2) +
		theme_bw() + labs(x=x.name, y=y.name, title="Median (+ IQR) Plot")

	histo = ggplot(data=m, aes(x=residuals)) + geom_histogram(fill='lightgray', col='black') + theme_bw() + labs(x=x.name, title="Histogram of Residuals")


	##### and a residual dependence plot
	res.dep = ggplot(data=m, aes(y=residuals, x=group)) + geom_jitter(alpha=.15, width=.05, size=.75) + stat_summary(fun.y=median, color="red", geom="line", aes(group=1)) + theme_bw() + labs(x=x.name, y="Absolute Value of Residuals", title="S-L Plot")
	
	##### put into a single plot
	plot_grid(t.test, histo, res.dep)

}

#' Print fifer.anova Summary
#'
#' Print fifer.anova Summary
#' @aliases print.fifer.anova
#' @param x a fifer.anova object
#' @param ... ignored
#' @export
print.fifer.anova = function(x,...){
	file.name = deparse(substitute(x))
	estimates.fifer.anova(x)
	cat(paste("\n\n Objects within this object:\n"))
	print(names(x))	
	cat(paste("\n\n To compute statistical significance, use the 'report' function. To see plots, use the 'plot' function."))	
}