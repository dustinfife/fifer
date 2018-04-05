#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @aliases visualize visualize.lm visualize.default
#' @param object a object
#' @export
visualize = function(object,...){
	UseMethod("visualize")
}

#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @aliases visualize visualize.lm visualize.default
#' @param object a object
#' @export
estimates.default = function(object){
	class(object) = "visualize"
	plot(object)
}

#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @aliases visualize visualize.lm visualize.default
#' @param object a object
#' @importFrom cowplot plot_grid
#' @export
visualize.lm = function(object, plot=c("bivariate", "residuals", "all"), linetype="loess"){
	

	plot = match.arg(plot, c("all", "residuals", "bivariate"))
	#### figure out what is numeric
	d = object$model
	levels = apply(d, 2, FUN=function(x) length(unique(x)))
	
	#### if there's too few levels and it's not categorical
	factors = !sapply(d, is.factor)
	if (any(levels<5 & factors)){
		cat("Note: one or more of your variables has less than 5 values, yet they're treated as numeric.\n\n")
	}
	
	#### extract names
	x.names = names(d)[-1] 
	y.name = names(d)[1]
	
	#### export residuals
	d$residuals = residuals(object)
	d$abs.res = abs(d$residuals)
	d$fitted = fitted(object)
	
	#### plot residuals
	histo = ggplot(data=d, aes(x=residuals)) + geom_histogram(fill='lightgray', col='black') + theme_bw() + labs(x="Residuals", title="Histogram of Residuals")
	res.dep = ggplot(data=d, aes(x=fitted, y=residuals)) + geom_point(alpha = .35, size=.75) + geom_smooth(method="loess") + 
		theme_bw() + labs(x="Fitted", y="Residuals", title="Residual Dependence Plot")
	sl = ggplot(data=d, aes(y=abs.res, x=fitted)) + geom_point(alpha=.35, size=.75) + geom_smooth(method= linetype) +
			theme_bw() + labs(x="fitted", y="Absolute Value of Residuals", title="S-L Plot")	
	
	
	#if (residuals){}

	#### if there's only two variables 
	if (ncol(d)==5){
		step3 = bivariate.plot(names(d)[1], names(d)[2], d=d)

	### if there's two variables and both variables are categorical
	} else if (ncol(d) == 6 & is.factor(d[,2]) & is.factor(d[,3])){
		step3=factorial.plot(d[,1], d[,2], d[,3], yname=y.name, x1name=x.names[1], x2name=x.names[2])
		
	
	### if there's two variables, one is numeric and one is categorical
	} else if (ncol(d) == 6 & any(!(factors[2:3]))){
		numeric = which(factors[2:3])+1
		factor = which(!(factors[2:3]))+1
		step3 = ggplot(data=d, aes_string(x=names(d)[numeric],y=names(d)[1], col=names(d)[factor])) +
			geom_point() + geom_smooth(method= linetype)	 + theme_bw()

	#### if there are two variables, one is numeric...
	} else if (ncol(d)==6 & all(factors[2:3])){
		#### divide second variable into groups
		custom.breaks = quantile(d[,3], c(.33, .66))
		x2.binned = cut(d[,3], breaks=c(-Inf, custom.breaks[1:2], Inf))
		ren = subsetString(levels(x2.binned), ",",2)
		ren = paste0("<", ren)
		ren = gsub("]", "", ren, fixed=T)
		ren[3] = paste0("<", max(d[,3]))
		levels(x2.binned) = ren
		d$x2.binned = x2.binned
		step3=ggplot(data=d, aes_string(x=names(d)[2], y=names(d)[1])) + 
			geom_point() + geom_smooth(method= linetype) + facet_grid(~x2.binned) + theme_bw()
	}

	#### return the plots
	if (plot=="bivariate"){
		return(step3)
	} else if (plot=="residuals"){
		plot_grid(histo, res.dep, sl)
	} else {
		plot_grid(step3, histo, res.dep, sl)
	}
}
