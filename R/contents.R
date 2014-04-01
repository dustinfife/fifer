##' View the contents (functions) of a package
##'
##' @details It's easy to forget what functions are contained within a package.
##' \code{contents} will return all the functions within a package
##' @param x a string containing the name of the package. Defaults to "fifer".
##' @return a string of all the functions contained within that package. 
##' @author Dustin Fife
##' @export
##' @examples
##' ## see what's in fifer
##' contents()
##' ## see what's in MASS
##' contents("MASS")
contents = function(x){
	p = paste0("package:", x)
	ls(p)
}