##' Read in a dataset and load the meta-data for a file
##'
##' Oftentimes the original data matrix is too large to practically read in everytime you want to do analysis.
##' This often means creating a separate file for analysis. Unfortunately, if the original file is changed, the separate
##' file doesn't reflect those changes. \code{read.fife} and \code{\link{write.fife}} both read and write meta-data,
##' then display the original file name for the meta data.
##'	
##' Technically, \code{read.fife} and \code{\link{write.fife}} don't actually read and write meta-data. Instead, they create
##' (or read) a separate file that has the same name (though different extension) than the subsetted dataset. The extension of 
##' the meta data file is .file. 
##' @param file The location of the file to be read.
##' @param file.type The extension of the dataset. Defaults to .csv.
##' @param ... other arguments passed to to \code{\link{read.csv}}
##' @aliases read.meta read.meta.data read.metadata read.fifer
##' @seealso \code{\link{write.fife}}
##' @return An R object containing the subsetted dataset. Also, an object called \code{original.file} will be loaded into the R 
##' environment that contains a string of the original file location
##' @author Dustin Fife
##' @export
read.fife = function(file, file.type = ".csv", ...){
	### set original.file to NULL so this passes R's check
	original.file = NULL
	data = read.csv(file, ...)
	#### extract the meta data
	meta = gsub(file.type, ".file", file, fixed=T)
	load(meta, verbose=T,envir=.GlobalEnv)
	cat(original.file)
	return(data)
}


##' Write a dataset and load the meta-data
##'
##' Oftentimes the original data matrix is too large to practically read in everytime you want to do analysis.
##' This often means creating a separate file for analysis. Unfortunately, if the original file is changed, the separate
##' file doesn't reflect those changes. \code{read.fife} and \code{\link{write.fife}} both read and write meta-data,
##' then display the original file name for the meta data.
##'	
##' Technically, \code{read.fife} and \code{\link{write.fife}} don't actually read and write meta-data. Instead, they create
##' (or read) a separate file that has the same name (though different extension) than the subsetted dataset. The extension of 
##' the meta data file is .file. 
##' @param object An R object to be written as a .csv (or whatever) file.
##' @param newfile The location of the subsetted dataset to be written.
##' @param originalfile The location of the original file that was subsetted. 
##' @param file.type The file type to be read. Defaults to .csv.
##' @param row.names Should row names be written? Defaults to FALSE.
##' @param fullpath Should the full path be written to the meta-data? (e.g., "documents/research/datasets/medical_data_ap9_2014.csv"). Defaults to T.
##' @param ... Other arguments passed to \code{\link{write.csv}}.
##' @aliases write.meta write.meta.data write.metadata write.fifer
##' @seealso \code{\link{read.fife}}
##' @author Dustin Fife
##' @export
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

	data = write.csv(object, fl, row.names=row.names, ...)
	
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
