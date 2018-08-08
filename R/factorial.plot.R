##' Plot a Factorial ANOVA
##'
##' Plot a Factorial ANOVA
##'	
##' This function plots the distribution of Y scores conditional on two factors. The first factor will occupy the x-axis, while the second will be displayed using different plots. 
##' @param y The outcome of interest
##' @param x1 The first factor. This will be plotted on the x-axis
##' @param x2 The second factor. This will be plotted in the columns of a plot grid
##' @return A plot
##' @author Dustin Fife
##' @export
factorial.plot = function(y, x1, x2, d, spread = c('quartiles', 'stdev', 'sterr'), raw.data=T, sample=Inf){
	
	#### sample, if needed
	if (sample==Inf){
		d.aes=""
	} else {
		samp = sample(1:nrow(d), size=sample)
		d.aes=paste0("data=data[", deparse(samp),",],")
	}
	#### name variables
	x.name = deparse(substitute(x1))
	x2.name = deparse(substitute(x2))
	y.name = deparse(substitute(y)) 

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


	#### plot with raw data
	if (!raw.data){
		call = paste0("ggplot(data=", deparse(substitute(d)), ", aes(x=",x1,", y=",y,", group=",x2,")) +
			stat_summary(data=d, fun.y='median', geom='line', aes(linetype=",x2,")) +
			stat_summary(aes(x=", x1, ", y=", y, "), geom='errorbar', fun.ymin=", ymin, ", fun.ymax = ", ymax, ", fun.y=", center, ", color='gray', width=.2, position=position_dodge(.15))+
			labs(x=x.name, y=y.name) +
			theme_bw()\n")

			
	} else {
		call = paste0("ggplot(data=", deparse(substitute(d)), ", aes(x=",x1,", y=",y,")) + 	
			geom_jitter(", d.aes, " alpha=.10, width=.2) + 
			facet_wrap(~", x2, ") + 
			stat_summary(aes(x=", x1, ", y=", y, "), geom='errorbar', fun.ymin=", ymin, ", fun.ymax = ", ymax, ", fun.y=", center, ", width=.2)+
			stat_summary(fun.y='", center, "', geom='line', group=1) + 
			labs(x=x.name, y=y.name) +
			theme_bw()\n"	)
			
			
			#### thiis looks good			
	}
	cat(paste0("R Code to Generate These Plots: \n\n"))
	cat(call)
	p <- eval(parse(text = call))			
	print(p)		
	return(p)






}

