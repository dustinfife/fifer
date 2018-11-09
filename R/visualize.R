#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @aliases visualize visualize.lm visualize.default
#' @param object a object
#' @param plot what should be plotted? Residuals? Bivariate plot? All of them?
#' @param formula A flexplot-style formula
#' @param linetype What time of smoothing line should be drawn? Defaults to loess. 
#' @export
visualize = function(object, plot=c("all", "residuals", "bivariate"),...){
	UseMethod("visualize")
}

#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @aliases visualize visualize.lm visualize.default
#' @param object a object
#' @param plot what should be plotted? Residuals? Bivariate plot? All of them?
#' @param formula A flexplot-style formula
#' @param linetype What time of smoothing line should be drawn? Defaults to loess. 
#' @export
estimates.default = function(object, plot=c("all", "residuals", "bivariate")){
	class(object) = "visualize"
	plot(object)
}

#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @aliases visualize visualize.lm visualize.default
#' @param object a lm object
#' @param plot what should be plotted? Residuals? Bivariate plot? All of them?
#' @param formula A flexplot-style formula
#' @param linetype What time of smoothing line should be drawn? Defaults to loess. 
#' @importFrom cowplot plot_grid
#' @export
visualize.lm = function(object, plot=c("all", "residuals", "bivariate"), linetype="loess"){
	

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
	
	
	#### use flexplot to visualize
	if (plot=="all" & is.null(formula)){
		warning("You must provide a formula argument to plot the data. I'm just returning the residual plots.")
	} else if (plot=="all"){
		step3 = flexplot(formula, data=d, ...)
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

#' Visualize a fitted model 
#'
#' Visualize a fitted model
#' @aliases visualize visualize.lmer visualize.default
#' @param object a lmer object
#' @param plot what should be plotted? Residuals? Bivariate plot? All of them?
#' @param formula A flexplot-style formula
#' @param linetype What time of smoothing line should be drawn? Defaults to loess. 
#' @importFrom cowplot plot_grid
#' @export
visualize.lmerMod = function(object, plot=c("residuals", "all", "bivariate"), linetype="loess", formula=NULL, ...){
	

	plot = match.arg(plot, c("all", "residuals", "bivariate"))
	#### figure out what is numeric
	d = object@frame
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
	

	#### use flexplot to visualize
	if (plot=="all" & is.null(formula)){
		warning("You must provide a formula argument to plot the data. I'm just returning the residual plots.")
	} else if (plot=="all"){
		step3 = flexplot(formula, data=d, ...)
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
