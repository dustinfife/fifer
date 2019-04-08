##' Clear memory of all objects
##'
##' This function is a wrapper for the command \code{rm(list=ls())}. It's just less keystrokes.
##'	
##' @param obj The object (as a string) that needs to be removed (or kept)
##' @param keep Should \code{obj} be kept (i.e., everything but \code{obj} removed)? Or dropped?
##' @author Dustin Fife
##' @export
##' @examples
##' #NOTE: ONLY RUN THIS IF YOU DON'T MIND ERASING YOUR R SESSION
##' a = 5
##' ls()	### a shows up
##' clear()
##' ls()	### nothing shows up
##' ### create objects
##' a=1; b=2; c=3; d=4; e=5
##' ### remove c
##' clear("c", keep=FALSE)
##' ls()
##' ### remove all but a and b
##' clear(c("a", "b"), keep=TRUE)
##' ls()
clear = function(obj=NULL, keep=TRUE){

	### remove everything if obj = null
	if (!is.null(obj)){
			### if they want to clear all but object
		if (keep){
			dropme = ls(envir=globalenv())[which(!(ls(envir=globalenv())%in%obj))]
		} else {
			dropme = obj
		}
		#print(dropme)
		rm(list=dropme, envir=globalenv())	
	} else {

		rm(list=ls(envir=globalenv()), envir=globalenv())
	}
}


