##' Drop (or keep) all columns containing a vector of strings
##'
##' @param string The string (or vector of strings) to be kept or omitted.
##' @param data The dataset
##' @param drop Logical. Should the columns containing \code{string} be dropped?
##' @seealso \code{\link{make.null}}
##' @references http://stackoverflow.com/questions/7597559/grep-in-r-with-a-list-of-patterns
##' @return a dataset with the column(s) dropped or kepts
##' @author Dustin Fife
##' @export
##' @examples
##' ### drop all columns with the words "Length"
##' data(iris)
##' iris.dropped = drop.columns("Length", data=iris, drop=TRUE)
##'
##' ### keep only those columns with HemoLeptin in the string
##' data(fakeMedicalData)
##' medical = drop.columns("HemoLeptin", data=fakeMedicalData, drop=FALSE)
##' head(medical)
drop.columns = function(string, data, drop=TRUE){
	omit = grep(paste(string, collapse="|"), names(data))
	if (drop){
		d = make.null(omit, data=data, keep=FALSE)
	} else {
		d = make.null(omit, data=data, keep=TRUE)
	}
	d
}	