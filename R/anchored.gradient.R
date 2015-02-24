##' Create a color gradient with a color for zero
##'
##' Create a color gradient with a color for zero
##'	
##' Other functions (e.g., \code{\link{number.to.colors}}) create a color gradient by taking a range (min and max) and begining with color 1 and ending with color 2.
##' The problem is that sometimes, there's a critical middle point (e.g., zero) and the min and max are not symmetric around the middle point.
##' This function aleviates that problem by creating a middle point for a fixed value (set to zero). 
##' @param minColor what color corresponds to the minimum value?
##' @param maxColor what color corresponds to the maximum value?
##' @param zeroColor what color corresponds to the middle point value?
##' @param z The vector of numbers corresponding to the colors?
##' @param vals how many points should the final gradient have?
##' @aliases color.gradient anchor.gradient
##' @seealso \code{\link{string.to.colors}}, \code{\link{number.to.colors}}, 
##' @return a gradient of color
##' @author Dustin Fife
##' @export
##' @examples
##' #not shown yet
anchored.gradient = function(minColor="blue", maxColor="red", zeroColor="white", z=NULL, vals=100){
	if (!is.null(zeroColor)){

		if (is.null(z)){
			min.max=c(-100, 100)
		} else {
			min.max = range(z)
		}
		### make sure vals is divisible by 2 (to put zero in the center)
		vals = ifelse(vals/2 == round(vals/2), vals, vals+1)	
		
		#### find min/max/zero colors
		min.rgb = col2rgb(minColor)/255
		zero.rgb = col2rgb(zeroColor)/255		
		max.rgb = col2rgb(maxColor)/255

		### compute assymetry
		direction.assy = ifelse(abs(min.max[1])>=abs(min.max[2]), "positive", "negative")
		if (direction.assy=="negative"){
			p.assy = abs(min.max[1])/sum(abs(min.max))
			assy = abs(min.max[1])/(abs(min.max[2]))
		} else {
			p.assy = abs(min.max[2])/sum(abs(min.max))
			assy = abs(min.max[2])/(abs(min.max[1]))
		}

		#### change min or max rgb
		if (direction.assy=="negative"){
			min.rgb = (1-assy)*zero.rgb + (assy)*min.rgb
		} else {
			max.rgb = (1-assy)*zero.rgb + (assy)*max.rgb
		}


		#### set the zero point (halfway between min and max)
		zero.points = vals/2
		
		#### create a function to go through the range of min.rgb, zero.rgb, and max.rgb
		f = function(i, mn, mx, length=3){seq(from=mn[i], to=mx[i], length.out=length)}
		if (direction.assy=="negative"){
			range.1 = matrix(unlist(lapply(1:3, FUN=f, mn=min.rgb, mx=zero.rgb, length=(p.assy)*vals)), nrow=3, byrow=T)
			num.1 = seq(from=min.max[1], to=0, length.out=(p.assy)*vals)
			range.2 = matrix(unlist(lapply(1:3, FUN=f, mn=zero.rgb, mx=max.rgb, length=(1-p.assy)*vals)), nrow=3, byrow=T)		
			num.2 = seq(from=0, to=min.max[2], length.out=(1-p.assy)*vals)
		} else {
			range.1 = matrix(unlist(lapply(1:3, FUN=f, mn=min.rgb, mx=zero.rgb, length=(1-p.assy)*vals)), nrow=3, byrow=T)
			num.1 = seq(from=min.max[1], to=0, length.out=(1-p.assy)*vals)
			range.2 = matrix(unlist(lapply(1:3, FUN=f, mn=zero.rgb, mx=max.rgb, length=(p.assy)*vals)), nrow=3, byrow=T)		
			num.2 = seq(from=0, to=min.max[2], length.out=(p.assy)*vals)
		}

		#### put in proper form		
		f2 = function(vec){rgb(vec[1], vec[2], vec[3])}
		range.1 = apply(range.1, 2, FUN=f2)
		range.2 = apply(range.2, 2, FUN=f2)		
		
		#### put into one vector
		colors = c(range.1, range.2)
		colors
		## test it
		#plot(c(num.1, num.2), c(num.1, num.2), col=colors)
		#abline(v=0)

	}
}