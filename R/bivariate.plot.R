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
bivariate.plot = function(x, y, d=NULL, jitter=FALSE, method="loess", spread=c('quartiles', 'stdev', 'sterr'), raw.data=T, sample=Inf, se=T, ...){

	length.x = length(unique(d[,x]))	
	length.y = length(unique(d[,y]))		
		
	#### specify conditions
	if (is.numeric(d[,x]) & length.x > 5){ 
			x.type="numeric"
	} else {
			x.type = "categorical"		
	}
		
	
	if (is.numeric(d[,y])){ 
		y.type="numeric"
	} else {
		y.type = "categorical"
	}
	
				##### big bug fixed via this answer: https://stackoverflow.com/questions/23563510/deparsesubstitute-within-function-using-data-table-as-argument
				##### I just got rid of every instance of d[,y] = factor(d[,y]) (or whatever)

	#### sample, if needed
	if (sample==Inf){
		d.aes=""
	} else {
		samp = sample(1:nrow(d), size=sample)
		d.aes=paste0("data=d[", deparse(samp),",],")
	}

			
	##### mean plot for one numeric
	if (x.type=="categorical" & y.type == "numeric" | x.type=="numeric" & y.type == "categorical"){
		#### flip x and y if y is categorical and x is not
		if (x.type=="numeric" & y.type == "categorical"){
			ynew = y;y = x;	x = ynew
		}
		
		#modify lower and upper limits, based on spread
		if (spread=="quartiles"){
			ymin = "function(z){quantile(z, .25)}"
			ymax = "function(z){quantile(z, .75)}"
			center = "median"
		} else if (spread=="stdev"){
			ymin = "function(z){mean(z)+sd(z)}"
			ymax = "function(z){mean(z)-sd(z)}"
			center = "mean"
		} else {
			ymin = "function(z){mean(z)-1.96*(sd(z)/sqrt(nrow(d)))}"
			ymax = "function(z){mean(z)+1.96*(sd(z)/sqrt(nrow(d)))}"			
			center = "mean"
		}


		
		#### plot raw data?
		
		if (raw.data){
			call = paste0("
			
			ggplot(data=", deparse(substitute(d)), ", aes(x=", x, ", y=", y, ")) +
				geom_jitter(", d.aes,", alpha=.15, width=.05, size=.75) +
				stat_summary(fun.y='", center, "', geom='point', size=2, color='red') +
				stat_summary(aes(x=", x, ", y=", y, "), geom='errorbar', fun.ymin=", ymin, ", fun.ymax = ", ymax, ", fun.y=", center, ", color='red', width=.2)+
				theme_bw()
				")
		} else {
			limits = range(d[,y], na.rm=T)
			call = paste0("
			
			ggplot(data=", deparse(substitute(d)), ", aes(x=", x, ", y=", y, ")) +
				stat_summary(fun.y='", center, "', geom='point', size=2, color='red') +
				stat_summary(aes(x=", x, ", y=", y, "), geom='errorbar', fun.ymin=", ymin, ", fun.ymax = ", ymax, ", fun.y=", center, ", color='red', width=.2)+
				coord_cartesian(ylim=limits) + 
				theme_bw()
				")			
		cat("Note: The Y axis has been scaled to the range of the available data.")
		}

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
			geom_jitter(", d.aes, ", width=.2) + geom_smooth(method=", method, ", se=", se, ") + theme_bw()
			") 			
		} else {
			call = paste0("
			
			ggplot(data=", deparse(substitute(d)), ", aes(x=", x, ", y=", y, ")) +
			geom_point(", d.aes, ") + geom_smooth(method=", method, ", se=", se, ") + theme_bw()
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