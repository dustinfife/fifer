##' Automatically convert a vector of strings into a color for easy plotting
##'
##' @title Convert between strings to colors
##' @param string a vector of strings representing groups. 
##' @param colors a vector of colors, one for each unique element in \code{string}.
##' @export
##' @return a vector of colors, one for each element in \code{string} 
##' @aliases string.to.colors stringtocolor stringToColors string.to.color
##' @author Dustin Fife
##' @seealso \code{\link{number.to.colors}}
##' @examples
##' groups = sample(LETTERS[1:5], size=100, replace=TRUE)
##' plot(rnorm(100), rnorm(100), col=string.to.colors(groups))
##' plot(rnorm(100), rnorm(100), col=string.to.colors(groups), 
##'    pch=as.numeric(string.to.colors(groups, colors=c(16:20))))
##' @note This function can also be used to specify pch values, cex values, or any other plotting values
##' the user may wish to differ across groups. See examples. 
string.to.colors = function(string, colors=NULL){
	if (is.factor(string)){
		string = as.character(string)
	}
	if (!is.null(colors)){
		if (length(colors)!=length(unique(string))){
			break("The number of colors must be equal to the number of unique elements.")
		}
		else {
			conv = cbind(unique(string), colors)
		}
	} else {
		conv = cbind(unique(string), rainbow(length(unique(string))))
	}
	unlist(lapply(string, FUN=function(x){conv[which(conv[,1]==x),2]}))
}


##' Automatically convert a vector of numbers into a color for easy plotting
##'
##' @title Convert from numbers to colors
##' @param value a vector of numbers. 
##' @param colors a vector of two or more colors representing the inflection points of the gradients, passed to \code{\link{colorRampPalette}}.
##' @param num The number of unique intervals for colors. Chose larger numbers for finer gradients (higher resolution).
##' @export
##' @return a vector of colors. 
##' @aliases number.to.color numbers.to.colors integers.to.colors integer.to.colors numberToColors numberToColor
##' @author Dustin Fife
##' @seealso \code{\link{string.to.colors}} \code{\link{colorRampPalette}} \code{\link{gradient.legend}}
##' @examples
##' #### plot three variables, represent the third with a color
##' d = mvrnorm(100, mu=c(0,0,0), Sigma=matrix(c(1, .6, .6, .6, 1, .6, .6, .6, 1), ncol=3))
##' plot(d[,1:2], col=number.to.colors(d[,3], c("black", "pink")), pch=16)
number.to.colors = function(value, colors=c("red", "blue"), num=100){
	cols = colorRampPalette(colors)(num)
	cols = 	cols[findInterval(value, vec=seq(from=min(value), to=max(value), length.out=num))]
	cols
}



##' Create a gradiented legend
##'	
##' @param y the variable used to create the gradient, typically in \code{\link{number.to.colors}}
##' @param yrange The range of y values. If y is supplied, it will pulls these from the actual y values.
##' @param cols The color gradients to be used that are passed to \code{\link{colorRampPalette}}.
##' @param location The location of the subplot, expressed in fractions of the entire plot (left x, right x,
##' bottom y, top y).
##' @param n the number of values used for the gradient. Higher numbers make a higher resolution
##' @param ... other arguments passed to image
##' @aliases gradient
##' @seealso \code{\link{number.to.colors}} \code{\link{colorRampPalette}}
##' @author Dustin Fife
##' @export
##' @examples
##' y = rnorm(100); x = .6*y + rnorm(100,0,sqrt(1-.6^2))
##' randnum = runif(100)
##' plot(x,y, col=number.to.colors(randnum), pch=16)
##' gradient.legend(randnum, xlab="", ylab="")
gradient.legend = function(y=NULL, yrange=NULL, cols = c("red", "blue"),location=c(.075,.3,.575,.975), n=100,...){
	
	#### if they don't supply a y, make sure they give range
	if (is.null(y) & is.null(yrange)){
		stop("You must supply either a y value or a y range.")
	}
	if (is.null(yrange)){
		yrange = range(y, na.rm=T)
	}
	
	#### set location
	par(fig=location, new=T, las=1, ps=9)
	z=matrix(1:n,nrow=1)
	x=1
	y=seq(yrange[1],yrange[2],len=n) 
	my.colors = colorRampPalette(cols)
	image(x,y,z,col=my.colors(n),axes=FALSE,...)
	axis(2)
	par(fig=c(0,1,0,1))
}