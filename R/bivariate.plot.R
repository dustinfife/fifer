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
##' @param ... Other arguments passed to ggplot functions
##' @seealso \code{\link{uni.plot}}
##' @return A plot
##' @author Dustin Fife
##' @export
##' @examples
##' x = rnorm(100)
##' y = rnorm(100)
##' bivariate.plot(x,y)
bivariate.plot = function(x, y, x.numeric=NULL, y.numeric=NULL, d=NULL,  ...){
	
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
		} else if (is.factor(x)){
			x.type = "categorical"; d[,x] = factor(d[,x])
		} else {
			x.type = "categorical"; d[,x] = factor(d[,x])
		}
		
	}
	if (!is.null(y.numeric)){
		y.type = ifelse(y.numeric==T, "numeric", "categorical")
	} else {
		if (is.numeric(d[,y]) & length.y > 5){ 
			y.type="numeric"
		} else if (is.factor(d[,y])){
			y.type = "categorical"; d[,y] = factor(d[,y])
		} else {
			y.type = "categorical"; d[,y] = factor(d[,y])
		}
		
	}
	
	

	##### scatterplot for both numeric
	if (x.type=="numeric" & y.type == "numeric"){
		require(tidyverse)
		p = ggplot(data=d, aes_string(x=x, y=y)) + geom_jitter(...) + geom_smooth(...)+ theme_bw()
		print(p)
	}
	

	
	##### mean plot for one numeric
	if (x.type=="categorical" & y.type == "numeric" | x.type=="numeric" & y.type == "categorical"){
		#### flip x and y if y is categorical and x is not
		if (x.type=="numeric" & y.type == "categorical"){
			ynew = y;y = x;	x = ynew
		}
	
		require(tidyverse)
		m = aggregate(d[,y]~d[,x], FUN=median); names(m) = c(x,y)
		upper = aggregate(d[,y]~d[,x], FUN =quantile, probs=.75); m$upper = upper[,ncol(upper)]
		lower = aggregate(d[,y]~d[,x], FUN =quantile,probs=.25); m$lower = lower[,ncol(lower)]

		p = ggplot(data=m, aes_string(x=x, y=y)) + geom_point(...) + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05) +
			geom_jitter(data=d, aes_string(x,y), alpha=.1, width=.05, size=.75,...)  + scale_x_discrete(labels=levels(d[,x]))
		print(p)

	}	
		#### if they specified both are categorical
	if ((x.type=="categorical" & y.type == "categorical")){
		require(tidyverse)
		m = as.data.frame(table(d[,x], d[,y])); names(m)[1:2] = c(x,y)
		Freq = "Freq"
		p = ggplot(data=m, aes_string(x=x, y=Freq, fill=y)) + geom_bar(stat="identity", position="dodge") 
		print(p)
	}		

}