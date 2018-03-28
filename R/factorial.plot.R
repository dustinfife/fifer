##' Plot a Factorial ANOVA
##'
##' Plot a Factorial ANOVA
##'	
##' This function plots the distribution of Y scores conditional on two factors. The first factor will occupy the x-axis, while the second will be displayed using different plots. 
##' @param y The outcome of interest
##' @param x1 The first factor. This will be plotted on the x-axis
##' @param x2 The second factor. This will be plotted in the columns of a plot grid
##' @return A plot
##' @author Dustin Fife
##' @export
factorial.plot = function(y, x1, x2){
	
	##### check if y/x are same length
	if ((length(x1) != length(y)) | (length(y) != length(x2))) {
        stop("Variables need to be the same length!")
    }	
    if (!is.factor(x1)){
	    x1 = factor(x1)
    }
    
    if (!is.factor(x2)){
    	x2 = factor(x2)
	}
	##### combine into gg-friendly dataframe
	m = data.frame(y = y, x1 = x1, x2=x2)
	
	#### handle missing values
	miss = which(is.na(m$y) | is.na(m$x1) | is.na(m$x2))
	if (length(miss) > 0) {
		warning("Missing values are being removed from the dataset.")
		m = m[-miss, ]
	}


	n = length(y)

	#### name variables
	x.name = deparse(substitute(x1))
	x.name = subsetString(x.name, "$", 2, T) 
	y.name = deparse(substitute(y)) 
	y.name = subsetString(y.name, "$", 2, T) 

	##### do a plot
	ggplot(data=m, aes(x=x1, y=y)) + geom_jitter(alpha = .35, size=.25, width=.2) + geom_smooth(method="loess") + 
		theme_bw() +  facet_grid(~x2)
		m
	ggplot(data=m, aes(x=x1, y=y)) + 	
		geom_jitter(alpha=.10, width=.2) + 
		facet_wrap(~x2) + 
		stat_summary(aes(x=x1, y= y), geom='errorbar', fun.ymin=function(z) {mean(z) - 1.96*(sd(z)/length(z))}, fun.ymax = function(z) {mean(z) + 1.96*(sd(z)/length(z))}, fun.y=mean,  width=.1) + 
		stat_summary(fun.y='mean', geom="line", group=1) + 
		labs(x=x.name, y=y.name, title="Factorial ANOVA") +
		theme_bw()		
}
