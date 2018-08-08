##' Plot univariate distribution of a variable
##'
##' A function that automatically plots either a barchart or a histogram (depending on data type)
##'	
##' Ideally, the user will provide the data type for this function (by saying numeric = T, or numeric = F)
##' Omitting that, the function will decide for the user. If the variable is coded as numeric AND there are more than five unique values of the variable,
##' it will generate a histogram. If the variable is a factor OR it has five or less unique values, it will output a barchart. 
##' @param variable A string indicating the name of the variable to plot
##' @param d The name of the dataset (which contains the variable the user wishes to plot)
##' @param numeric Logical. Is the variable of interest numeric? (Meaning, should a histogram be plotted?)
##' @seealso \code{\link{MASS}}
##' @return A plot
##' @author Dustin Fife
##' @export
##' @import tidyverse
##' @examples
##' distress = sample(1:10, size=22, replace=T)
##' uni.plot(distress, numeric=T, d=NULL)
uni.plot = function(variable, data=NULL, ...){
	
	#levels = unique(data[,variable])

	#### identify whether it's numeric
	if (is.numeric(data[,variable])){
		
		#### create histogram
		q = ggplot(data=data, aes_string(variable)) + geom_histogram(fill="lightgray", col="black") + theme_bw() + labs(x=variable)
		
		### now create the code that created it
		output = paste0("R Code to generate plots: \n\n ggplot(data=", deparse(substitute(data)), ", aes(", variable, ")) + geom_histogram(fill='lightgray', col='black') + theme_bw() + labs(x='", variable, "')\n")
		cat(output)
		return(q)			
		
	} else {
		
		##### create bargraph
		p = ggplot(data=data, aes_string(variable)) + geom_bar() + theme_bw() + labs(x=variable)
		output = paste0("R Code to generate plots: \n\n ggplot(data=", deparse(substitute(data)), ", aes(", variable, ")) + geom_bar() + theme_bw() + labs(x='", variable, "')\n")
		cat(output)	
		p


	} 
		#### this seems to be a bug in ggplot. I have to print p (without using "return") outside of the else function
		#### see more at https://github.com/tidyverse/ggplot2/issues/2514, though this just let me to trial and error until I got it to work
	#p
}