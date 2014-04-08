##' Load the meta-data for a file
##'
##' \code{read.fife}
##'	
##' Write the details here
##' @param
##' @param
##' @aliases
##' @seealso \code{\link{MASS}}
##' @references
##' @return \item{}{}
##' @author
##' @export
##' @examples
read.fife = function(file, file.type = ".csv", ...){
	data = read.csv(file, ...)
	#### extract the meta data
	meta = gsub(file.type, ".file", file, fixed=T)
	loaded.data = load(meta, verbose=T)
	cat(original.file)
}


write.fife = function(object, newfile, originalfile=NULL, file.type = ".csv", row.names=F, fullpath=T,...){
	
	#### bark if they don't supply originalfile name
	if (is.null(originalfile)){
		stop('You must supply an argument for originalfile.')
	}

	#### if they write "file.csv" do nothing, otherwise add .csv to the end
	if (length(grep(file.type, newfile))>0){
		fl = newfile
	} else {
		fl = paste0(newfile, file.type)				
	}

	data = write.csv(object, fl, row.names, ...)
	
	#### format the 
	meta = gsub(file.type, ".file", fl, fixed=T)
	
	#### give only individual file name (if specified)
	if (!fullpath){
		k = unlist(strsplit(originalfile, "/"))
		originalfile = k[length(k)]		
	}
	
	#### make an object of the meta file
	original.file = paste0("Original File Name: ", originalfile)
	save(original.file, file=meta)
}
