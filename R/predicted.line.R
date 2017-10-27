##' Create and plot a fitted line for a generalized linear model
##'
##' Create and plot a fitted line for a generalized linear model
##'	
##' @param model A model (e.g., lmer model, or any type of model that supports the "predict" command)
##' @param outcome A string that names the outcome variable
##' @param IV A string that names the predictor variable
##' @param data The original dataset used to fit the model
##' @param covariates A function used on all the covariates. Can be mean, median, min, or max
##' @param length The number of predicted points used for plotting the line. Defaults to 100. 
##' @param Add. Should the fitted line be added to an existing plot?
##' @param ... Other arguments passed to plot, lines, or predict
##' @author Dustin Fife
##' @export
##' @examples
##' ## do this later
predicted.line = function(model, outcome, IV, covs = NA, data, cov.func=c("mean", "median", "min", "max"), length=100, add=F,...){

	d = data
	##### extract terms
	dv = all.vars(model$terms)[1]
	ivs = all.vars(model$terms)[2:length(all.vars(model$terms))]	

	##### create new dataset (with same names as d)
	new.dat = d[1:100,c(dv, ivs)]

	### mathc argument
	covariates=match.arg(cov.func, c("mean", "median", "min", "max"))

	#### repeat the mean for all variables
	means = apply(new.dat, 2, covariates)
	new.dat = data.frame(matrix(means, nrow=100, ncol=ncol(new.dat), byrow=T))		
	names(new.dat) = c(dv, ivs)
	
	#### replace IV
	iv.vals = d[,IV]
	new.dat[,IV] = seq(from=min(iv.vals, na.rm=T), to=max(iv.vals, na.rm=T), length.out=100)
	
	#### predict DV
	new.dat[,outcome] = predict(model, new.dat, ...)
	
	##### plot the line
	if (!add){
		plot(iv.vals, dv.vals, ...)
	}
	lines(new.dat[,IV], new.dat[,outcome], ...)
}