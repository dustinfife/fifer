##' Plot an two-way quantitative interaction in an image plot
##'
##' Plot an two-way quantitative interaction in an image plot
##'	
##' The \code{\link{image}} function is quite useful for displaying two-way quantitative interactions (variable X on the X axis,
##' variable Y on the Y axis, and z represented as a color). However, this function requires a matrix where the cells (r_ij) represent the Z value
##' in the ith value of x and jth value of y. Getting it into matrix form can be annoying if it natively comes in long-column format. This function
##' takes a data matrix, conditions as well as z in the columns, and returns an image matrix.
##' @param data a data matrix with columns indicating the values of x, y, and z
##' @param x the name of the x variable (a string)
##' @param y the name of the y variable (a string)
##' @param z the name of the z variable (a string)
##' @param ... other arguments passed to \code{\link{anchored.gradient}}.
##' @param plot Should an image be plotted?
##' @param legend add a legend to the plot?
##' @importFrom fields image.plot
##' @seealso \code{\link{image}}
##' @return the x range, y range, and image matrix
##' @author Dustin Fife
##' @export
##' @examples
##' ## do this later
imageInteraction = function(data, x, y, z, plot=TRUE, legend=TRUE,...){
	x.range = sort(unique(data[,x]))
	y.range = sort(unique(data[,y]))
	image = matrix(nrow=length(x.range), ncol=length(y.range))
	i=1;j=1
	for (i in 1:nrow(image)){
		for (j in 1:ncol(image)){
			rws = which(data[,x]==x.range[i] & data[,y]==y.range[j])
			k = data[rws,]
			image[i,j] = mean(k[,z], na.rm=T)
		}
	}
	
	### plot that foo
	if (plot){
		if (legend){
			image.plot(x.range, y.range, image,  
							col = anchored.gradient(minColor="red", maxColor="blue", zeroColor="white", z=c(image), vals=100),...)
		} else {
			image(x.range, y.range, image,  
				col = anchored.gradient(minColor="red", maxColor="blue", zeroColor="white", z=c(image)),...)

		}
	}	
	list(x.range, y.range, image)		
}