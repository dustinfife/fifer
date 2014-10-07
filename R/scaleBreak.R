##' Scatterplot with a Scale Break 
##'
##' With skewed data, important relationships are often scaled so small because the graph attempts to capture the outliers.
##' \code{scaleBreak} will plot two different scales on the same plot, shrinking the range where the outliers lay. The break
##' in scale is indicated with a mark.
##'	
##' @param x The x variables to be plotted.
##' @param y The y variables to be plotted.
##' @param axis Which axis should have two different scales. Currently, only implemented for "2," which is the y axis.
##' @param breakpos At what point should the break occur? Defaults to 1.
##' @param plot.numbers Which plots to include. Can be 1, 2, or c(1,2).
##' @param ... other arguments passed to plot
##' @importFrom plotrix axis.break
##' @author Dustin Fife
##' @export
##' @aliases scale.break scalebreak scale.Break
##' @examples
##' # generate correlated data
##' d = data.frame(mvrnorm(1000, mu=c(0,0), Sigma=matrix(c(1,.6,.6,1), nrow=2)))
##' names(d) = c("x","y")
##' ## Skew Y
##' d$y = d$y^2
##' scaleBreak(d$x, d$y, breakpos=4, plot.numbers=1)
##' ## add a lowess line
##' lines(lowess(d$x, d$y), col="red")
##' ## add a fitted line
##' mod = glm(y~x, data=d, family=inverse.gaussian)
##' curve(predict(mod,data.frame(x=x),type="resp"),add=TRUE,col="blue")
##' ## add second plot
##' scaleBreak(d$x, d$y, breakpos=4, plot.numbers=2)
scaleBreak = function(x,y,axis=2, breakpos=1, plot.numbers=c(1,2),...){
	
	#### figure out which Y values are above the breakpos
	y_above = y[y>breakpos]; x_above = x[y>breakpos]
	y_below = y[y<=breakpos]; x_below = x[y<=breakpos]
	
	#### pick ranges
	range_1 = range(y_below)
	range_2 = range(y_above)
	
	#### find limits of y axis (so it spans 2/3rds)
	mx = max(y_below); mn = min(y_below)
	ylims1 = c(mn, mx + (mx-mn)/2)

	#### plot bottom graph (if specified)
	if (1 %in% plot.numbers){
		plot(range(x), ylims1, type="n", yaxt="n",...)
		points(x_below, y_below, yaxt="n",...)
		l = pretty(y_below)
		axis(2, l[1:(length(l)-1)])
	}
	
	min2 = (breakpos + .45*breakpos - max(y))/.45

	if (2 %in% plot.numbers){
		#### add second graph
		par(new=TRUE)
		plot(range(x), c(min2, max(y)), type="n", yaxt="n", xaxt="n", ylab="", axes=F, xlab="")
		points(x_above, y_above, yaxt="n",...)
		axis(2, pretty(y_above)[-1])
		axis.break(axis=axis, breakpos)				
	}
}
