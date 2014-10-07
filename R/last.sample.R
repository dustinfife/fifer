##' Return only one row per ID
##'
##' Often times, an individual has multiple observations in the dataset. \code{last.sample} will
##' loop through the entire dataset and return only one observation per individual, giving the
##' first (or last) draw for a person, or performing some function on the variable of interest.
##'	
##' @param ID The name of the unique identifier, expressed as a string (e.g., "Match.Group") 
##' @param sort.var The variable to be sorted on in order to take the first (or last) sample, expressed as a string.
##' @param decreasing How should the sort.var variable be sorted? Defaults to T.
##' @param data The dataset with multiple rows per individual
##' @param FUN What should be done with the multiple samples? This function can be used to extract the last 
##' (or first) sample using the decreasing/sort.var options, or a function can be performed (such as the mean)
##' on one or more columns. See examples. 
##' @param fun.var If FUN if not null, the variable (or a vector of variables), expressed as strings to have the function
##' applied to.
##' @param ... Other arguments passed to the chosen function.
##' @aliases lastsample lastSample one.row last.row
##' @return a new dataframe containing one row per ID
##' @author Dustin Fife
##' @export
##' @examples
##' #### take only group 2 values
##' last.sample(ID="ID", sort.var="group", data=sleep)
##' #### take only group 1 values
##' last.sample(ID="ID", sort.var="group", decreasing=FALSE,data=sleep)
##' #### average group 1 and 2 values
##' last.sample(ID="ID", data=sleep, FUN=mean, fun.var="extra")
##' #### take the maximum extra value
##' last.sample(ID="ID", data=sleep, FUN=max, fun.var="extra")
##' #### take the mean of two columns extra value
##' sleep$group = as.numeric(as.character(sleep$group))
##' last.sample(ID="ID", data=sleep, FUN=mean, fun.var=c("group","extra"))
last.sample = function(ID, sort.var=NULL, decreasing=TRUE, data, FUN=NULL, fun.var=NULL,...){

	#### if they gave a function but not a variable (or vice versa), bark
	if (is.null(FUN) & !is.null(fun.var) | !is.null(FUN) & is.null(fun.var)){
		stop("Both FUN and fun.var must either be null or not null.")
	}

	#### first sort by specified variable
	if (!is.null(sort.var)){
		data = data[order(data[,ID],data[,sort.var], decreasing=decreasing),]	
	}
	
	#### extract unique IDs
	IDs = unique(data[,ID])
	
	#### preallocate
	new.dat = data[1:length(IDs),]
	
	#### loop
	for (i in 1:nrow(new.dat)){
		k = data[data[,ID]==IDs[i],]
	
		new.dat[i,] = k[1,]
		
		### if they specified a FUN, use it on the variable specified
		if (is.null(fun.var)){
			new.dat[i,fun.var] = k[1,]
		} else if (length(fun.var)==1){
			new.dat[i,fun.var] = FUN(k[,fun.var],...)
		} else {
			new.dat[i,fun.var] = apply(k[,fun.var], 2, FUN=FUN,...)
		}	
	}
	
	return(new.dat)
}