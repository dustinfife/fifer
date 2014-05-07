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
##' @import plotrix
##' @aliases scale.break scalebreak scale.Break
##' @author Dustin Fife
##' @export
##' @examples
#' # generate correlated data
#' d = data.frame(mvrnorm(1000, mu=c(0,0), Sigma=matrix(c(1,.6,.6,1), nrow=2)))
#' names(d) = c("x","y")
#' ## Skew Y
#' d$y = d$y^2
#' scaleVreak(d$x, d$y, breakpos=4)
scaleBreak = function(x,y,axis=2, breakpos=1,...){
	
	#### figure out which Y values are above the breakpos
	y_above = y[y>breakpos]; x_above = x[y>breakpos]
	y_below = y[y<=breakpos]; x_below = x[y<=breakpos]
	
	#### pick ranges
	if (is.null(range_1)){range_1 = range(y_below)}
	if (is.null(range_2)){range_2 = range(y_above)}	
	
	#### find limits of y axis (so it spans 2/3rds)
	mx = max(y_below); mn = min(y_below)
	ylims1 = c(mn, mx + (mx-mn)/2)

	#### plot bottom graph
	plot(range(x), ylims1, type="n", yaxt="n",...)
	points(x_below, y_below, yaxt="n", col="blue",...)
	axis(2, pretty(y_below))
	#axis.break(axis=axis, breakpos,style="zigzag")
	
	min2 = (breakpos + .45*breakpos - max(y))/.45
	
	#### add second graph
	par(new=TRUE)
	plot(range(x), c(min2, max(y)), type="n", yaxt="n", ylab="", axes=F,...)
	points(x_above, y_above, yaxt="n",col="red",...)
	axis(2, pretty(y_above)[-1])

	axis.break(axis=axis, breakpos)		
}

