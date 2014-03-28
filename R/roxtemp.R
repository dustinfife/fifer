##' Generate a Roxygen Template
##'
##' @title Generate a Roxygen Template
##' @author Dustin Fife
##' @export
roxtemp = function(){
	f = paste0(
"##' Make a title
##'
##' @title
##' @param
##' @param
##' @return \\item{}{}
##' @author
##' @export
##' @examples"
)
	cat(f)
}