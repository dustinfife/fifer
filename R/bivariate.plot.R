##' Plot bivariate plot of two variables
##'
##' A function that automatically plots either a scatterplot, mean plot, or a barchart
##'	
##' Ideally, the user will provide the data type for this function (by saying numeric = T, or numeric = F)
##' Omitting that, the function will decide for the user. If both variables are numeric (or if there are more than five unique values of the variable)
##' it will generate a scatterplot. If one variable is a factor OR it has five or less unique values, it will output a mean plot. If all
##' variables are categorical, it will output a barchart.  
##' @param x A string indicating the name of the IV to plot. Alternatively, an object can be used. 
##' @param y A string indicating the name of the DV to plot. Alternatively, an object can be used. 
##' @param x.numeric Logical. Is the predictor variable of interest numeric?
##' @param y.numeric Logical. Is the outcome variable of interest numeric?
##' @param jitter Logical. Should variables be jittered?
##' @param ... Other arguments passed to ggplot functions
##' @seealso \code{\link{uni.plot}}
##' @return A plot
##' @author Dustin Fife
##' @export
##' @import tidyverse
##' @examples
##' x = rnorm(100)
##' y = rnorm(100)
##' bivariate.plot(x,y)
bivariate.plot = function(x, y, x.numeric=NULL, y.numeric=NULL, d=NULL,  jitter=FALSE, ...){
	
	require(tidyverse)
	#### first try to find the variable
		if (is.null(d) & (is.character(x) | is.character(y))){
			stop("You must specify a dataset if you surround the variable name in quotes")
		} else if (!is.null(d)){
			#### try finding that variable in the dataset
			out = tryCatch(ncol(d[,c(x,y)])>0, error=function(error){paste0("I couldn't find '", x, "' or '", y , "' in your dataset"); return(FALSE)})			
			if (!out){
				stop(paste0("I couldn't find either '", x, "' or '", y , "' in your dataset"))
			}
		} else if (is.null(d[,c(x,y)]) & !is.character(x) & !is.character(y)){
			d = data.frame(cbind(x, y)); names(d) = c(deparse(substitute(x)), deparse(substitute(y)))
			x = deparse(substitute(x))
			y = deparse(substitute(y))			
		}



	length.x = length(unique(d[,x]))	
	length.y = length(unique(d[,y]))		
		
	#### specify conditions
	if (!is.null(x.numeric)){
		x.type = ifelse(x.numeric==T, "numeric", "categorical")
	} else {
		if (is.numeric(d[,x]) & length.x > 5){ 
			x.type="numeric"
		} else {
			x.type = "categorical"
			
		}
		
	}
	if (!is.null(y.numeric)){
		y.type = ifelse(y.numeric==T, "numeric", "categorical")
	} else {
		if (is.numeric(d[,y]) & length.y > 5){ 
			y.type="numeric"
		} else {
			y.type = "categorical"
		}
		
	}
	
				##### big bug fixed via this answer: https://stackoverflow.com/questions/23563510/deparsesubstitute-within-function-using-data-table-as-argument
				##### I just got rid of every instance of d[,y] = factor(d[,y]) (or whatever)
	
	##### mean plot for one numeric
	if (x.type=="categorical" & y.type == "numeric" | x.type=="numeric" & y.type == "categorical"){
		#### flip x and y if y is categorical and x is not
		if (x.type=="numeric" & y.type == "categorical"){
			ynew = y;y = x;	x = ynew
		}
		call = paste0("
		
		ggplot(data=d", ", aes(x=", x, ", y=", y, ")) +
			geom_jitter(alpha=.15, width=.05, size=.75) +
			stat_summary(fun.y='median', geom='point', size=2, color='red') +
			stat_summary(aes(x=", x, ", y=", y, "), geom='errorbar', fun.ymin=function(z) {quantile(z, .25)}, fun.ymax = function(z) {quantile(z, .75)}, fun.y=median, color='red', width=.2)
			")
		cat(paste0("R Code to Generate These Plots: \n\n"))
		cat(call)
		p <- eval(parse(text = call))			
		print(p)		
		return(p)
	}		

	##### scatterplot for both numeric
	if (x.type=="numeric" & y.type == "numeric"){
		
		if (jitter){
			call = paste0("
			
			ggplot(data=", deparse(substitute(d)), ", aes(x=", x, ", y=", y, ")) +
			geom_jitter(width=.2) + geom_smooth() + theme_bw()
			") 			
		} else {
			call = paste0("
			
			ggplot(data=", deparse(substitute(d)), ", aes(x=", x, ", y=", y, ")) +
			geom_point() + geom_smooth() + theme_bw()
			") 
		}
		cat(paste0("R Code to Generate These Plots: \n\n"))
		cat(call)
		p <- eval(parse(text = call))			
		print(p)
		return(p)
	}
	

	

		#### if they specified both are categorical
	if ((x.type=="categorical" & y.type == "categorical")){
		call = paste0("
		m = as.data.frame(table(", deparse(substitute(d)), "[,'", x, "'],", deparse(substitute(d)), "[,'", y, "'])); names(m)[1:2] = c('", x, "', '", y, "')
		Freq = 'Freq'
		ggplot(data=m, aes(x=", x, ", y=Freq, fill=", y, ")) + geom_bar(stat='identity', position='dodge')
		")
		p <- eval(parse(text = call))			
		print(p)
		cat(paste0("R Code to Generate These Plots: \n\n"))
		cat(call)		
		return(p)
	}		

}