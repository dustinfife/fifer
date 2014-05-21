##' Automatically convert a vector of strings into a color for easy plotting
##'
##' @title Convert between strings to colors
##' @param string a vector of strings representing groups. 
##' @param colors a vector of colors, one for each unique element in \code{string}.
##' @export
##' @return a vector of colors, one for each element in \code{string} 
##' @aliases string.to.colors stringtocolor stringToColors string.to.color
##' @author Dustin Fife
##' @examples
##' groups = sample(LETTERS[1:5], size=100, replace=TRUE)
##' plot(rnorm(100), rnorm(100), col=string.to.color(groups))
##' plot(rnorm(100), rnorm(100), col=string.to.color(groups), 
##'    pch=as.numeric(string.to.color(groups, colors=c(16:20))))
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
##' @param colors a vector of two or more colors representing the inflection points of the gradients.
##' @num The number of unique intervals for colors. Chose larger numbers for finer gradients (higher resolution).
##' @export
##' @return a vector of colors. 
##' @aliases number.to.color numbers.to.colors integers.to.colors integer.to.colors numberToColors numberToColor
##' @author Dustin Fife
##' @examples
##' #### plot three variables, represent the third with a color
##' d = mvrnorm(100, mu=c(0,0,0), Sigma=matrix(c(1, .6, .6, .6, 1, .6, .6, .6, 1), ncol=3))
##' plot(d[,1:2], col=number.to.colors(d[,3], c("black", "pink")), pch=16)
number.to.colors = function(value, colors=c("red", "blue"), num=100){
	cols = colorRampPalette(colors)(num)
	cols = 	cols[findInterval(value, vec=seq(from=min(value), to=max(value), length.out=num))]
	cols
}
